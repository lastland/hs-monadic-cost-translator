-- | Identify declaration roles in the typechecked home graph.
module ConstructionRoles
    ( QuickCheck, quickCheckRoles, isQuickCheckBinding, quickCheckBody
    , supportCoreNames, homeDemandTypes
    , Support, supportRoles, supportAt, supportBinding, sourceBinding
    ) where

import ConstructionAst
import ConstructionQuickCheckTypes (checkedHome)
import ConstructionRuntime
import Data.List (nub)
import Data.Foldable (toList)
import Data.Data (Data, cast)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import GHC
import GHC.Core.FamInstEnv (FamInst(..))
import qualified GHC.Core as C
import GHC.Core.Class (classAllSelIds, classTyCon)
import GHC.Core.DataCon (dataConWorkId)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.Type (splitTyConApp_maybe, splitFunTys, splitForAllTyVars)
import GHC.Core.TyCon (tyConSingleDataCon)
import GHC.Types.Id (isDFunId)
import qualified GHC.Data.Strict as Strict
import GHC.Tc.Types (tcg_tcs, tcg_fam_insts)
import GHC.Types.SrcLoc (getBufSpan)
import GHC.Types.Name.Set (NameSet)

-- Permission belongs to the nominal class field, never its dictionary or
-- spelling. Every nested declaration starts a fresh ordinary obligation.
data Support = Support [(SrcSpan, Maybe [Role])] (Map.Map Name [Role]) (Set.Set Name)

supportRoles :: Runtime -> QuickCheck -> [(TypecheckedModule, C.CoreProgram)] -> Support
supportRoles rt qc programs = Support regions core declared
  where
    modules = map fst programs
    roots = [(getLocA n, role) | m <- modules, Just (group, _, _, _, _) <- [tm_renamed_source m],
        ClsInstDecl{cid_binds = bs} <- collectAst group :: [ClsInstDecl GhcRn],
        L _ (FunBind _ n _) <- toList bs, Just role <- [roleOf rt (unLoc n)],
        role `elem` [AbstractMethod, FullMethod, UsageMethod]]
    bindings = [(m, b) | m <- modules, Just (group, _, _, _, _) <- [tm_renamed_source m],
        b <- collectAst group :: [HsBind GhcRn]]
    methods = [getLocA n | m <- modules, Just (group, _, _, _, _) <- [tm_renamed_source m],
        inst <- collectAst group :: [ClsInstDecl GhcRn], L _ (FunBind _ n _) <- toList (cid_binds inst)] ++
        [getLocA n | m <- modules, Just (group, _, _, _, _) <- [tm_renamed_source m],
        ClassDecl{tcdMeths = bs} <- collectAst group :: [TyClDecl GhcRn],
        L _ (FunBind _ n _) <- toList bs]
    helper (FunBind _ n _) = isExternalName (unLoc n) && getLocA n `notElem` methods
    helper _ = False
    references b = concat (children supportReferences b)
    edges = Map.fromList [(n, references b) | (_, b) <- bindings, helper b, Just n <- [bindingName b]]
    reached role = reachable edges (Set.fromList [n | (_, b@(FunBind _ binder _)) <- bindings,
        lookup (getLocA binder) roots == Just role, n <- references b])
    candidates = Set.unions (map reached [AbstractMethod, FullMethod, UsageMethod])
    ordinary = reachable edges (Set.fromList [n | (m, b) <- bindings,
        not (quickCheckBody qc m b), not (maybe False (`Set.member` candidates) (bindingName b) && helper b),
        not (case b of FunBind _ binder _ -> getLocA binder `elem` map fst roots; _ -> False),
        n <- references b])
    helperRoles n = [role | role <- [AbstractMethod, FullMethod, UsageMethod],
        n `Set.member` reached role, n `Set.notMember` ordinary, Map.member n edges]
    sourceRole n = case lookup (getLocA n) roots of
        Just role -> Just [role]
        Nothing -> case helperRoles (unLoc n) of [] -> Nothing; roles -> Just roles
    regions = [(getLocA b, case unLoc b of
            FunBind _ n _ -> sourceRole n
            _ -> Nothing) | m <- modules,
        Just (group, _, _, _, _) <- [tm_renamed_source m],
        b <- collectAst group :: [LHsBind GhcRn]]
    typed = [(getName (unLoc n), getLocA n) | m <- modules, n <- functionBinders m]
    exports = [(getName poly, getName mono) | m <- modules,
        ABE poly mono _ _ <- collectAst (tm_typechecked_source m)]
    fields = [(getName method, role) | (_, program) <- programs, (df, rhs) <- C.flattenBinds program, isDFunId df,
        Just (cls, _) <- [getClassPredTys_maybe (snd (splitFunTys (snd (splitForAllTyVars (idType df)))))],
        let (_, body) = C.collectBinders rhs, (C.Var con, args) <- [C.collectArgs body],
        con == dataConWorkId (tyConSingleDataCon (classTyCon cls)),
        let values = filter (not . C.isTyCoArg) args,
        length values == length (classAllSelIds cls),
        (selector, value) <- zip (classAllSelIds cls) values,
        Just role <- [roleOf rt (getName selector)], role `elem` [AbstractMethod, FullMethod, UsageMethod],
        (C.Var method, _) <- [C.collectArgs value]]
    sourceRoles = [(getLocA n, roles) | (_, FunBind _ n _) <- bindings, Just roles <- [sourceRole n]]
    initial = Map.fromList ([(name, [role]) | (name, role) <- fields] ++
        [(name, roles) | (name, at) <- typed, Just roles <- [lookup at sourceRoles]])
    close names = let next = Map.union names (Map.fromList
                         [(poly, role) | (poly, mono) <- exports, Just role <- [Map.lookup mono names]])
                  in if Map.size next == Map.size names then names else close next
    core = close initial
    sourceNames = [getName v | m <- modules, b <- collectAst (tm_typechecked_source m) :: [HsBind GhcTc],
        v <- collectHsBindBinders CollNoDictBinders b]
    declared = Set.fromList (sourceNames ++ [poly | (poly, mono) <- exports, mono `elem` sourceNames])

supportAt :: Support -> SrcSpan -> Maybe [Role]
supportAt (Support regions _ _) at = case
    [(region, role) | (region, role) <- regions, at `isSubspanOf` region,
        not (any (\(inner, _) -> inner /= region && inner `isSubspanOf` region && at `isSubspanOf` inner) regions)] of
    [(_, role)] -> role
    _ -> Nothing

supportBinding :: Support -> Name -> Maybe [Role]
supportBinding (Support _ names _) name = Map.lookup name names

sourceBinding :: Support -> Name -> Bool
sourceBinding (Support _ _ names) name = Set.member name names

-- References belong to the immediate equation. Nested definitions, including
-- unused ones, have their own role and cannot borrow their parent's authority.
supportReferences :: Data a => a -> [Name]
supportReferences value
    | Just (_ :: HsBind GhcRn) <- cast value = []
    | Just (_ :: NameSet) <- cast value = []
    | Just (HsVar _ n) <- cast value :: Maybe (HsExpr GhcRn) = [unLoc n]
    | Just (HsRecSel _ field) <- cast value :: Maybe (HsExpr GhcRn) = [foExt field]
    | otherwise = concat (children supportReferences value)

-- Only the result after the binders counts: Gen in an argument, or inside
-- MemoristM, does not turn a translated computation into a generator.
newtype QuickCheck = QuickCheck (Set.Set Name)

isQuickCheckBinding :: NamedThing a => QuickCheck -> a -> Bool
isQuickCheckBinding (QuickCheck names) = (`Set.member` names) . getName

quickCheckRoles :: (Module -> Type -> Bool) -> [TypecheckedModule] -> Ghc QuickCheck
quickCheckRoles result modules = do
    let declared = Set.fromList [name | m <- modules, Just (group, _, _, _, _) <- [tm_renamed_source m],
            binding <- collectAst group, Just name <- [bindingName binding]]
        -- A renamed method refers to its class selector. The typechecked binds
        -- and their exports identify its implementations, and also name local
        -- generators. Do not grant a role to an arbitrary intermediate Core
        -- expression just because its type happens to be Gen.
        locals m = map unLoc (functionBinders m) ++
            [v | ABE poly mono _ _ <- collectAst (tm_typechecked_source m), v <- [poly, mono]]
    things <- mapM lookupName (filter isExternalName (Set.toList declared))
    let ids = [(nameModule (getName v), v) | Just (AnId v) <- things] ++
            [(checkedHome m, v) | m <- modules, v <- locals m]
    pure (QuickCheck (Set.fromList [getName v | (home, v) <- ids, result home (idType v)]))

-- One selector can have both a Gen-valued implementation and an ordinary one.
-- Match the implementation at its binder token in this module's input buffer;
-- never grant uses of the general selector the role of a particular instance.
quickCheckBody :: QuickCheck -> TypecheckedModule -> HsBind GhcRn -> Bool
quickCheckBody qc checked = exempt
  where
    typed = Map.fromListWith (&&)
        [((offset, getOccName v), isQuickCheckBinding qc v)
        | n <- functionBinders checked, let v = unLoc n,
          Strict.Just offset <- [getBufSpan (getLocA n)]]
    exempt binding
        | Just n <- bindingName binding, isQuickCheckBinding qc n = True
        | FunBind _ n _ <- binding, Strict.Just offset <- getBufSpan (getLocA n) =
            Map.findWithDefault False (offset, getOccName (unLoc n)) typed
        | otherwise = False

functionBinders :: TypecheckedModule -> [LocatedN Id]
functionBinders checked = [n | FunBind _ n _ <- collectAst (tm_typechecked_source checked) :: [HsBind GhcTc]]

-- These facts only reject executable Eq/Show/etc. uses on demand data. An
-- actual associated type equation is not authority to observe a thunk.
homeDemandTypes :: Runtime -> [TypecheckedModule] -> [TyCon]
homeDemandTypes rt checked = nub
    [tc | m <- checked, equation <- tcg_fam_insts (fst (tm_internals_ m)),
        fi_fam equation == runtimeName rt "Memorist.Data.Interface.Demand" "Dem",
        Just (tc, _) <- [splitTyConApp_maybe (fi_rhs equation)], tc `elem` homeTypes]
  where homeTypes = [tc | m <- checked, tc <- tcg_tcs (fst (tm_internals_ m))]

-- GHC gives instance methods fresh internal identifiers. Follow only the finite
-- AbsBinds export relation, not name spellings or IdInfo/unfoldings.
supportCoreNames :: [(SrcSpan, Name)] -> TypecheckedModule -> [Name]
supportCoreNames support checked = close initial
  where
    initial = [getName (unLoc name) | name <- functionBinders checked, getLocA name `elem` map fst support]
    exports = [(getName poly, getName mono) | ABE poly mono _ _ <- collectAst (tm_typechecked_source checked) :: [ABExport]]
    close names = let more = names ++ mapMaybe (\(poly, mono) -> if mono `elem` names && poly `notElem` names then Just poly else Nothing) exports
                  in if length more == length names then names else close more
