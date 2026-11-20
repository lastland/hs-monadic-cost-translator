-- | Keep a QuickCheck expression's consumed local values in that expression.
-- Independently callable helpers and unused definitions still have bodies.
module ConstructionLocals (Locals, quickCheckBindings, checkedActions, SourceBody(..), SourceSite(..), localBindings, implicitBodies,
    nestedSource, ordinaryCoreBindings, components, coreLocation) where

import ConstructionAst (bindingName, children, collectAst, constructorPattern, nestedBindings, strictNullaryRhs, typedVariable)
import ConstructionActionFields (ActionFields, actionFields, inModule, sourceActions)
import ConstructionRoles (QuickCheck, isQuickCheckBinding)
import ConstructionRuntime (Role(Payload), Runtime, roleOf, runtimeName)
import Control.Monad (zipWithM)
import Data.Data (Data, cast)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC
import GHC.Builtin.Types (unitDataCon)
import qualified GHC.Core as C
import GHC.Core.Coercion (coercionLKind, coercionRKind, topNormaliseNewType_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCon (isDataTyCon)
import GHC.Core.Type (isPiTy, isTyVarTy, splitTyConApp_maybe)
import GHC.Hs.Syn.Type (hsExprType, lhsExprType)
import GHC.Tc.Types.Evidence (HsWrapper(..))
import GHC.Types.Basic (Boxity(Boxed))
import GHC.Types.Name (isInternalName)
import GHC.Types.Name.Set (NameSet)

data Locals = Locals
    { quickCheckBindings :: QuickCheck, expressionValues :: Set.Set Name, independentIds :: Set.Set Name
    , ipSites :: Map.Map Name SrcSpan, checkedActions :: TypecheckedModule -> ActionFields }

data SourceSite = MethodAt SrcSpan Name | PatternAt SrcSpan | ComponentAt SrcSpan SrcSpan

data SourceBody = Declaration (HsBind GhcRn) | Component (LPat GhcRn) (LHsExpr GhcRn)

-- Stop at each declaration. A mention in a nested shrinker is a use by that
-- shrinker, not a use by the generator that encloses both of them.
parts :: Data a => a -> ([HsBind GhcRn], [Name])
parts node
    -- The enclosing expression forces this RHS without a variable occurrence.
    -- A strict name can also have ordinary consumers; a binderless force cannot.
    | Just b <- cast node, forcedBinder b = case names b of
        [] -> bodyParts b
        ns -> ([b], ns)
    | Just binding <- cast node = ([binding], [])
    | Just (_ :: IPBind GhcRn) <- cast node = mempty
    | Just (HsVar _ n) <- cast node :: Maybe (HsExpr GhcRn) = ([], [unLoc n])
    | Just (_ :: NameSet) <- cast node = mempty
    | Just (_ :: Name) <- cast node = mempty
    | otherwise = mconcat (children parts node)

forcedBinder :: HsBind GhcRn -> Bool
forcedBinder PatBind{pat_lhs = pat} = strictPat pat
  where
    strictPat (L _ (ParPat _ p)) = strictPat p
    -- Matching boxed unit only forces the RHS; it has no fields or method evidence.
    strictPat (L _ (BangPat _ p)) = simpleBinder p || maybe False null (constructorPattern (getName unitDataCon) p)
    strictPat _ = False
forcedBinder FunBind{fun_matches = matches} = strictNullaryRhs matches
forcedBinder _ = False

simpleBinder :: LPat (GhcPass p) -> Bool
simpleBinder (L _ (ParPat _ p)) = simpleBinder p
simpleBinder (L _ (SigPat _ p _)) = simpleBinder p
simpleBinder (L _ VarPat{}) = True
simpleBinder (L _ WildPat{}) = True
simpleBinder _ = False

bodyParts :: HsBind GhcRn -> ([HsBind GhcRn], [Name])
bodyParts = mconcat . children parts

names :: HsBind GhcRn -> [Name]
names = collectHsBindBinders CollNoDictBinders

-- These are the actual fields of a constructed, boxed tuple. A variable in
-- one position does not select its siblings. Keep guards, unknown shared RHSs
-- and patterns that do additional matching as one declaration.
components :: HsBind (GhcPass p) -> Maybe [(LPat (GhcPass p), LHsExpr (GhcPass p))]
components PatBind{pat_lhs = pat, pat_rhs = GRHSs _ [L _ (GRHS _ [] rhs)] (EmptyLocalBinds _)} = tuple pat rhs
  where
    tuple (L _ (ParPat _ p)) e = tuple p e
    tuple p (L _ (HsPar _ e)) = tuple p e
    tuple (L _ (TuplePat _ ps Boxed)) (L _ (ExplicitTuple _ args Boxed))
        | length ps == length args = concat <$> zipWithM field ps args
    tuple _ _ = Nothing
    field p (Present _ e) | simpleBinder p = Just [(p, e)]
                          | otherwise = tuple p e
    field _ _ = Nothing
components _ = Nothing

-- An explicit ?binding defines a dynamically scoped dictionary. Its RHS has
-- ordinary obligations even if the surrounding property never selects it.
implicitBodies :: HsGroup GhcRn -> [LHsExpr GhcRn]
implicitBodies group = [rhs | IPBind _ _ rhs <- collectAst group]

localBindings :: Runtime -> QuickCheck -> [TypecheckedModule] -> Locals
localBindings rt qc modules = Locals qc values (Set.union (Map.keysSet dictionaries) (reachable aliases independent)) dictionaries (inModule actions)
  where
    groups = [group | m <- modules, Just (group, _, _, _, _) <- [tm_renamed_source m]]
    roots = [bodyParts b | group <- groups, b <- fst (parts group)]
    ips = [parts rhs | group <- groups, rhs <- implicitBodies group]
    actions = actionFields rt qc modules
    actionRefs = concat [snd (parts rhs) | fields <- Map.elems actions, rhs <- sourceActions fields]
    local b = concat [(b, ns, refs) : concatMap local nested | (ns, (nested, refs)) <- regions]
      where
        regions = case components b of
            Just fields -> [(collectPatBinders CollNoDictBinders p, parts e) | (p, e) <- fields]
            Nothing -> [(names b, bodyParts b)]
    locals = concatMap (concatMap local . fst) (roots ++ ips)
    typed = map tm_typechecked_source modules
    exports = [(getName poly, getName mono) | m <- typed, ABE poly mono _ _ <- collectAst m]
    aliases = Map.fromListWith (++) ([(p, [m]) | (p, m) <- exports] ++ [(m, [p]) | (p, m) <- exports])
    valueIds = dataBinders rt aliases typed
    generator b = maybe False (isQuickCheckBinding qc) (bindingName b)
    -- A matched field has its own binders. Only an indivisible RHS must
    -- qualify all its binders together.
    value FunBind{fun_matches = matches} _ | matchGroupArity matches > 0 = False
    value b ns = not (null ns) && all (`Set.member` valueIds) ns && not (generator b)
    deps = Map.fromList [(n, refs) | (b, ns, refs) <- locals, value b ns, n <- ns]
    -- Propagate only through values. Calling or passing a named function never
    -- gives its body the caller's role. Ordinary code still checks its entire
    -- RHS; these sets only choose where to resume *inside* an exempt RHS.
    live = reachable deps (Set.fromList (concatMap snd roots ++ concat [refs | (b, _, refs) <- locals, generator b]))
    consumed ns = not (null ns) && all (`Set.member` live) ns
    ordinary = reachable deps (Set.fromList (actionRefs ++ concatMap snd ips ++ [ref | (b, ns, refs) <- locals, not (generator b),
        not (value b ns && consumed ns), ref <- refs]))
    values = Set.fromList [n | (b, ns, _) <- locals, value b ns, consumed ns, all (`Set.notMember` ordinary) ns, n <- ns]
    independent = Set.fromList [n | (_, ns, _) <- locals, n <- ns, Set.notMember n values]
    dictionaries = Map.fromList [(getName v, getLocA label) | m <- typed, IPBind v label _ <- collectAst m :: [IPBind GhcTc]]

data Result = DataValue | Executable | Unknown deriving Eq

-- Type/dictionary abstractions and translated actions are work, even when
-- their final result is Bool. A newtype around a function is not scalar data.
result :: Runtime -> Type -> Result
result rt ty
    | isPiTy ty || computation ty || isPiTy rep || computation rep = Executable
    | Just (tc, _) <- splitTyConApp_maybe rep, isDataTyCon tc || isPrimTyCon tc = DataValue
    | otherwise = Unknown
  where
    computation t = maybe False ((== runtimeName rt "Memorist.Base.ANT" "MemoristT") . getName . fst) (splitTyConApp_maybe t)
    rep = maybe ty snd (topNormaliseNewType_maybe ty)

dataBinders :: Runtime -> Map.Map Name [Name] -> [TypecheckedSource] -> Set.Set Name
dataBinders rt aliases typed = Set.fromList [getName v | b <- binds, v <- collectHsBindBinders CollNoDictBinders b,
    case result rt (idType v) of
        DataValue -> True
        Unknown -> Set.member (getName v) witnessed && Set.notMember (getName v) executed
        Executable -> False]
  where
    binds = concatMap collectAst typed :: [HsBind GhcTc]
    witnessed = reachable aliases (Set.fromList [getName v | FunBind _ n matches <- binds, let v = unLoc n,
        result rt (idType v) == Unknown, matchGroupArity matches == 0,
        let rhss = [rhs | L _ m <- unLoc (mg_alts matches), L _ (GRHS _ _ rhs) <- grhssGRHSs (m_grhss m)],
        not (null rhss), all (valueRhs rt (idType v) . unLoc) rhss])
    executed = reachable aliases (Set.fromList [n | m <- typed, e <- collectAst m, Just (_, n) <- [typedVariable e],
        Set.member n witnessed, result rt (lhsExprType e) == Executable])

-- Inspect only a definition's result wrapper. An abstract payload may also be
-- the existing value of an enclosing thunk, projected at its checked type.
-- Neither a different binder's cast nor an arbitrary callee is a witness.
valueRhs :: Runtime -> Type -> HsExpr GhcTc -> Bool
valueRhs rt ty = at
  where
    at (XExpr (WrapExpr (HsWrap w e))) = castToResult w || at e
    at (HsPar _ e) = at (unLoc e)
    at (ExprWithTySig _ e _) = at (unLoc e)
    at e@(HsApp _ f x) | isTyVarTy ty, eqType ty (hsExprType e),
        Just (_, selector) <- typedVariable f, roleOf rt selector == Just Payload,
        Just (_, input) <- typedVariable x = isInternalName input
    at _ = False
    castToResult (WpCompose outer inner) = castToResult outer || castToResult inner
    castToResult (WpCast co) = eqType ty (coercionRKind co) && result rt (coercionLKind co) == DataValue
    castToResult _ = False

reachable :: Map.Map Name [Name] -> Set.Set Name -> Set.Set Name
reachable edges initial = go initial (Set.toList initial)
  where
    go seen [] = seen
    go seen (n:rest) = let next = Set.fromList (Map.findWithDefault [] n edges) `Set.difference` seen
                       in go (Set.union seen next) (Set.toList next ++ rest)

-- This is entered only when the existing declaration-role check has already
-- exempted the parent. Bindings without a proven value dependency are checked.
nestedSource :: Locals -> HsBind GhcRn -> [SourceBody]
nestedSource locals = concatMap boundary . fst . bodyParts
  where
    boundary b | Just fields <- components b, any (covered . fst) fields = concatMap field fields
               | forcedBinder b, Just n <- bindingName b, isQuickCheckBinding (quickCheckBindings locals) n =
                   nestedSource locals b
               | consumed (names b) = nestedSource locals b
               | otherwise = [Declaration b]
    consumed ns = not (null ns) && all (`Set.member` expressionValues locals) ns
    covered = consumed . collectPatBinders CollNoDictBinders
    field (p, e) | covered p = concatMap boundary (fst (parts e))
                 | otherwise = [Component p e]

-- The ordinary passes already traverse ordinary RHSs, stopping at QuickCheck
-- binders. Add just the independent source bodies encountered below those
-- stops. Desugaring's administrative lets inherit their expression's role.
ordinaryCoreBindings :: Locals -> [(Id, C.CoreExpr)] -> [(Id, C.CoreExpr)]
ordinaryCoreBindings locals bs = filter (not . isQuickCheckBinding qc . fst) bs ++ concatMap (inside False) bs
  where
    qc = quickCheckBindings locals
    inside testing pair@(v, rhs) =
        let exempt = isQuickCheckBinding qc v
            resume = testing && not exempt && Set.member (getName v) (independentIds locals)
        in [pair | resume] ++ concatMap (inside (exempt || (testing && not resume))) (nestedBindings rhs)

-- GHC's IP dictionary Id can have no source span; its declaration has one.
coreLocation :: Locals -> Id -> SrcSpan
coreLocation locals v = Map.findWithDefault (nameSrcSpan n) n (ipSites locals) where n = getName v
