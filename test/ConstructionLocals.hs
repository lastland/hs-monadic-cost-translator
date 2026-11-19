-- | Keep a QuickCheck expression's consumed local values in that expression.
-- Independently callable helpers and unused definitions still have bodies.
module ConstructionLocals (Locals, SourceBody(..), SourceSite(..), localBindings, nestedSource, ordinaryCoreBindings, components) where

import ConstructionAst (bindingName, children, collectAst, nestedBindings)
import ConstructionRoles (QuickCheck, isQuickCheckBinding)
import Control.Monad (zipWithM)
import Data.Data (Data, cast)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC
import qualified GHC.Core as C
import GHC.Core.Coercion (coercionLKind, coercionRKind)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (isFunTy, splitTyConApp_maybe)
import GHC.Tc.Types.Evidence (HsWrapper(..))
import GHC.Tc.Utils.TcType (tcSplitNestedSigmaTys)
import GHC.Types.Basic (Boxity(Boxed))
import GHC.Types.Name.Set (NameSet)

data Locals = Locals { expressionValues :: Set.Set Name, independentIds :: Set.Set Name }

data SourceSite = MethodAt SrcSpan Name | PatternAt SrcSpan | ComponentAt SrcSpan SrcSpan

data SourceBody = Declaration (HsBind GhcRn) | Component (LPat GhcRn) (LHsExpr GhcRn)

-- Stop at each declaration. A mention in a nested shrinker is a use by that
-- shrinker, not a use by the generator that encloses both of them.
parts :: Data a => a -> ([HsBind GhcRn], [Name])
parts node
    -- A bang-wildcard has no independently usable name. Evaluating the
    -- enclosing expression enters this RHS even with no variable occurrence.
    | Just b@PatBind{pat_lhs = pat} <- cast node, forcedWildcard pat = bodyParts b
    | Just binding <- cast node = ([binding], [])
    | Just (HsVar _ n) <- cast node :: Maybe (HsExpr GhcRn) = ([], [unLoc n])
    | Just (_ :: NameSet) <- cast node = mempty
    | Just (_ :: Name) <- cast node = mempty
    | otherwise = mconcat (children parts node)

forcedWildcard :: LPat GhcRn -> Bool
forcedWildcard (L _ (ParPat _ pat)) = forcedWildcard pat
forcedWildcard (L _ (BangPat _ pat)) = wildcard (unLoc pat)
  where
    wildcard WildPat{} = True
    wildcard (ParPat _ p) = wildcard (unLoc p)
    wildcard _ = False
forcedWildcard _ = False

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
    field p (Present _ e) | simple p = Just [(p, e)]
                          | otherwise = tuple p e
    field _ _ = Nothing
    simple (L _ (ParPat _ p)) = simple p
    simple (L _ VarPat{}) = True
    simple (L _ WildPat{}) = True
    simple _ = False
components _ = Nothing

localBindings :: QuickCheck -> [TypecheckedModule] -> Locals
localBindings qc modules = Locals values (reachable aliases independent)
  where
    roots = [bodyParts b | m <- modules, Just (group, _, _, _, _) <- [tm_renamed_source m], b <- fst (parts group)]
    local b = concat [(b, ns, refs) : concatMap local nested | (ns, (nested, refs)) <- regions]
      where
        regions | value b, Just fields <- components b =
                    [(collectPatBinders CollNoDictBinders p, parts e) | (p, e) <- fields]
                | otherwise = [(names b, bodyParts b)]
    locals = concatMap (concatMap local . fst) roots
    typed = map tm_typechecked_source modules
    exports = [(getName poly, getName mono) | m <- typed, ABE poly mono _ _ <- collectAst m]
    aliases = Map.fromListWith (++) ([(p, [m]) | (p, m) <- exports] ++ [(m, [p]) | (p, m) <- exports])
    -- A point-free helper can have an opaque family signature. Keep the
    -- function shape witnessed at its typechecked definition or use; never
    -- reduce potentially recursive families or interpret the helper itself.
    asFunctions = concat [functionCasts w | m <- typed, w <- collectAst m]
    functionCasts (WpCompose outer inner) = functionCasts outer ++ functionCasts inner
    functionCasts (WpCast co) = [t | let l = coercionLKind co; r = coercionRKind co,
        (t, other) <- [(l, r), (r, l)], isFunTy other]
    functionCasts _ = []
    callable t = isFunTy t || (maybe True (isTypeFamilyTyCon . fst) (splitTyConApp_maybe t) && any (eqType t) asFunctions)
    valueIds = Set.fromList [getName v | m <- typed, binding <- collectAst m :: [HsBind GhcTc],
        v <- collectHsBindBinders CollNoDictBinders binding,
        let (_, _, result) = tcSplitNestedSigmaTys (idType v), not (callable result)]
    generator b = maybe False (isQuickCheckBinding qc) (bindingName b)
    value FunBind{fun_matches = matches} | matchGroupArity matches > 0 = False
    value b = not (null ns) && all (`Set.member` valueIds) ns && not (generator b) where ns = names b
    deps = Map.fromList [(n, refs) | (b, ns, refs) <- locals, value b, n <- ns]
    -- Propagate only through values. Calling or passing a named function never
    -- gives its body the caller's role. Ordinary code still checks its entire
    -- RHS; these sets only choose where to resume *inside* an exempt RHS.
    live = reachable deps (Set.fromList (concatMap snd roots ++ concat [refs | (b, _, refs) <- locals, generator b]))
    consumed ns = not (null ns) && all (`Set.member` live) ns
    ordinary = reachable deps (Set.fromList [ref | (b, ns, refs) <- locals, not (generator b),
        not (value b && consumed ns), ref <- refs])
    values = Set.fromList [n | (b, ns, _) <- locals, value b, consumed ns, all (`Set.notMember` ordinary) ns, n <- ns]
    independent = Set.fromList [n | (_, ns, _) <- locals, n <- ns, Set.notMember n values]

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
               | consumed (names b) = nestedSource locals b
               | otherwise = [Declaration b]
    consumed ns = not (null ns) && all (`Set.member` expressionValues locals) ns
    covered = consumed . collectPatBinders CollNoDictBinders
    field (p, e) | covered p = concatMap boundary (fst (parts e))
                 | otherwise = [Component p e]

-- The ordinary passes already traverse ordinary RHSs, stopping at QuickCheck
-- binders. Add just the independent source bodies encountered below those
-- stops. Desugaring's administrative lets inherit their expression's role.
ordinaryCoreBindings :: Locals -> QuickCheck -> [(Id, C.CoreExpr)] -> [(Id, C.CoreExpr)]
ordinaryCoreBindings locals qc bs = filter (not . isQuickCheckBinding qc . fst) bs ++ concatMap (inside False) bs
  where
    inside testing pair@(v, rhs) =
        let exempt = isQuickCheckBinding qc v
            resume = testing && not exempt && Set.member (getName v) (independentIds locals)
        in [pair | resume] ++ concatMap (inside (exempt || (testing && not resume))) (nestedBindings rhs)
