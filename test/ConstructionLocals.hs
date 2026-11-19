-- | Keep a QuickCheck expression's consumed local values in that expression.
-- Independently callable helpers and unused definitions still have bodies.
module ConstructionLocals (Locals, SourceSite(..), localBindings, implicitBodies, nestedSource, ordinaryCoreBindings, coreLocation) where

import ConstructionAst (bindingName, children, collectAst, nestedBindings, typedVariable)
import ConstructionRoles (QuickCheck, isQuickCheckBinding)
import ConstructionRuntime (Role(Payload), Runtime, roleOf, runtimeName)
import Data.Data (Data, cast)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC
import qualified GHC.Core as C
import GHC.Core.Coercion (coercionLKind, coercionRKind, topNormaliseNewType_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCon (isDataTyCon)
import GHC.Core.Type (isPiTy, isTyVarTy, splitTyConApp_maybe)
import GHC.Hs.Syn.Type (hsExprType, lhsExprType)
import GHC.Tc.Types.Evidence (HsWrapper(..))
import GHC.Types.Name (isInternalName)
import GHC.Types.Name.Set (NameSet)

data Locals = Locals { expressionValues :: Set.Set Name, independentIds :: Set.Set Name, ipSites :: Map.Map Name SrcSpan }

data SourceSite = MethodAt SrcSpan Name | PatternAt SrcSpan

-- Stop at each declaration. A mention in a nested shrinker is a use by that
-- shrinker, not a use by the generator that encloses both of them.
parts :: Data a => a -> ([HsBind GhcRn], [Name])
parts node
    | Just binding <- cast node = ([binding], [])
    | Just (_ :: IPBind GhcRn) <- cast node = mempty
    | Just (HsVar _ n) <- cast node :: Maybe (HsExpr GhcRn) = ([], [unLoc n])
    | Just (_ :: NameSet) <- cast node = mempty
    | Just (_ :: Name) <- cast node = mempty
    | otherwise = mconcat (children parts node)

bodyParts :: HsBind GhcRn -> ([HsBind GhcRn], [Name])
bodyParts = mconcat . children parts

names :: HsBind GhcRn -> [Name]
names = collectHsBindBinders CollNoDictBinders

-- An explicit ?binding defines a dynamically scoped dictionary. Its RHS has
-- ordinary obligations even if the surrounding property never selects it.
implicitBodies :: HsGroup GhcRn -> [LHsExpr GhcRn]
implicitBodies group = [rhs | IPBind _ _ rhs <- collectAst group]

localBindings :: Runtime -> QuickCheck -> [TypecheckedModule] -> Locals
localBindings rt qc modules = Locals values (Set.union (Map.keysSet dictionaries) (reachable aliases independent)) dictionaries
  where
    groups = [group | m <- modules, Just (group, _, _, _, _) <- [tm_renamed_source m]]
    roots = [bodyParts b | group <- groups, b <- fst (parts group)]
    ips = [parts rhs | group <- groups, rhs <- implicitBodies group]
    local b = (b, refs) : concatMap local nested where (nested, refs) = bodyParts b
    locals = concatMap (concatMap local . fst) (roots ++ ips)
    typed = map tm_typechecked_source modules
    exports = [(getName poly, getName mono) | m <- typed, ABE poly mono _ _ <- collectAst m]
    aliases = Map.fromListWith (++) ([(p, [m]) | (p, m) <- exports] ++ [(m, [p]) | (p, m) <- exports])
    valueIds = dataBinders rt aliases typed
    generator b = maybe False (isQuickCheckBinding qc) (bindingName b)
    value FunBind{fun_matches = matches} | matchGroupArity matches > 0 = False
    value b = not (null ns) && all (`Set.member` valueIds) ns && not (generator b) where ns = names b
    deps = Map.fromList [(n, refs) | (b, refs) <- locals, value b, n <- names b]
    -- Propagate only through values. Calling or passing a named function never
    -- gives its body the caller's role. Ordinary code still checks its entire
    -- RHS; these sets only choose where to resume *inside* an exempt RHS.
    live = reachable deps (Set.fromList (concatMap snd roots ++ concat [refs | (b, refs) <- locals, generator b]))
    usedBy set b = any (`Set.member` set) (names b)
    ordinary = reachable deps (Set.fromList (concatMap snd ips ++ [ref | (b, refs) <- locals, not (generator b),
        not (value b && usedBy live b), ref <- refs]))
    values = Set.fromList [n | (b, _) <- locals, value b, usedBy live b, not (usedBy ordinary b), n <- names b]
    independent = Set.fromList [n | (b, _) <- locals, n <- names b, Set.notMember n values]
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
nestedSource :: Locals -> HsBind GhcRn -> [HsBind GhcRn]
nestedSource locals = concatMap boundary . fst . bodyParts
  where
    boundary b | not (null ns) && all (`Set.member` expressionValues locals) ns = nestedSource locals b
               | otherwise = [b]
      where ns = names b

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

-- GHC's IP dictionary Id can have no source span; its declaration has one.
coreLocation :: Locals -> Id -> SrcSpan
coreLocation locals v = Map.findWithDefault (nameSrcSpan n) n (ipSites locals) where n = getName v
