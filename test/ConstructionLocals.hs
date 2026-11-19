-- | Keep a QuickCheck expression's consumed local values in that expression.
-- Independently callable helpers and unused definitions still have bodies.
module ConstructionLocals (Locals, quickCheckBindings, SourceSite(..), localBindings, nestedSource, ordinaryCoreBindings) where

import ConstructionAst (bindingName, children, collectAst, nestedBindings)
import ConstructionRoles (QuickCheck, isQuickCheckBinding)
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
import GHC.Types.Name.Set (NameSet)

data Locals = Locals { quickCheckBindings :: QuickCheck, expressionValues :: Set.Set Name, independentIds :: Set.Set Name }

data SourceSite = MethodAt SrcSpan Name | PatternAt SrcSpan

-- Stop at each declaration. A mention in a nested shrinker is a use by that
-- shrinker, not a use by the generator that encloses both of them.
parts :: Data a => a -> ([HsBind GhcRn], [Name])
parts node
    | Just binding <- cast node = ([binding], [])
    | Just (HsVar _ n) <- cast node :: Maybe (HsExpr GhcRn) = ([], [unLoc n])
    | Just (_ :: NameSet) <- cast node = mempty
    | Just (_ :: Name) <- cast node = mempty
    | otherwise = mconcat (children parts node)

bodyParts :: HsBind GhcRn -> ([HsBind GhcRn], [Name])
bodyParts = mconcat . children parts

names :: HsBind GhcRn -> [Name]
names = collectHsBindBinders CollNoDictBinders

localBindings :: QuickCheck -> [TypecheckedModule] -> Locals
localBindings qc modules = Locals qc values (reachable aliases independent)
  where
    roots = [bodyParts b | m <- modules, Just (group, _, _, _, _) <- [tm_renamed_source m], b <- fst (parts group)]
    local b = (b, refs) : concatMap local nested where (nested, refs) = bodyParts b
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
    deps = Map.fromList [(n, refs) | (b, refs) <- locals, value b, n <- names b]
    -- Propagate only through values. Calling or passing a named function never
    -- gives its body the caller's role. Ordinary code still checks its entire
    -- RHS; these sets only choose where to resume *inside* an exempt RHS.
    live = reachable deps (Set.fromList (concatMap snd roots ++ concat [refs | (b, refs) <- locals, generator b]))
    usedBy set b = any (`Set.member` set) (names b)
    ordinary = reachable deps (Set.fromList [ref | (b, refs) <- locals, not (generator b),
        not (value b && usedBy live b), ref <- refs])
    values = Set.fromList [n | (b, _) <- locals, value b, usedBy live b, not (usedBy ordinary b), n <- names b]
    independent = Set.fromList [n | (b, _) <- locals, n <- names b, Set.notMember n values]

reachable :: Map.Map Name [Name] -> Set.Set Name -> Set.Set Name
reachable edges initial = go initial (Set.toList initial)
  where
    go seen [] = seen
    go seen (n:rest) = let next = Set.fromList (Map.findWithDefault [] n edges) `Set.difference` seen
                       in go (Set.union seen next) (Set.toList next ++ rest)

-- This is entered only when the existing declaration-role check has already
-- exempted the parent. Bindings without a proven value dependency are checked.
nestedSource :: Locals -> HsBind GhcRn -> [HsBind GhcRn]
nestedSource locals = concatMap boundary . fst . bodyParts
  where
    boundary b | not (null ns) && all (`Set.member` expressionValues locals) ns = nestedSource locals b
               | otherwise = [b]
      where ns = names b

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
