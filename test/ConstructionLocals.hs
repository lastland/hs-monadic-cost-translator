-- | Keep a QuickCheck expression's consumed local values in that expression.
-- Independently callable helpers and unused definitions still have bodies.
module ConstructionLocals (Locals, quickCheckBindings, checkedActions, SourceBody(..), SourceSite(..), localBindings, implicitSource,
    implicitBindingAt, nestedSource, ordinaryCoreBindings, components, coreLocation) where

import ConstructionAst (bindingName, checkedModule, children, collectAst, constructorPattern, firstOrderCall, nestedBindings, reachable, strictNullaryRhs, typedVariable, variable)
import ConstructionActionFields (ActionFields, actionFields, inModule, sourceActions)
import ConstructionImplicit
import ConstructionRoles (QuickCheck, isQuickCheckBinding, quickCheckBody)
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
import GHC.Types.Name (isInternalName, nameModule_maybe)
import GHC.Types.Name.Set (NameSet)

data Locals = Locals
    { quickCheckBindings :: QuickCheck, expressionValues :: Set.Set Name, independentIds :: Set.Set Name
    , ipSites :: Map.Map Name SrcSpan, moduleIPs :: Map.Map Module Implicits
    , checkedActions :: TypecheckedModule -> ActionFields }

data SourceSite = MethodAt SrcSpan Name | PatternAt SrcSpan | ComponentAt SrcSpan SrcSpan | ImplicitAt SrcSpan

data SourceBody = Declaration (HsBind GhcRn) | Component (LPat GhcRn) (LHsExpr GhcRn) | Implicit (LHsExpr GhcRn)

-- Stop at each declaration. A mention in a nested shrinker is a use by that
-- shrinker, not a use by the generator that encloses both of them.
parts :: Data a => (LHsExpr GhcRn -> [Reference]) -> a -> ([HsBind GhcRn], [Reference])
parts mentions node
    -- The enclosing expression forces this RHS without a variable occurrence.
    -- A strict name can also have ordinary consumers; a binderless force cannot.
    | Just b <- cast node, forcedBinder b = case names b of
        [] -> bodyParts mentions b
        ns -> ([b], map Direct ns)
    | Just binding <- cast node = ([binding], [])
    | Just (_ :: IPBind GhcRn) <- cast node = mempty
    | Just e@(L _ HsIPVar{}) <- cast node :: Maybe (LHsExpr GhcRn) = ([], mentions e)
    | Just e@(L _ (HsVar _ n)) <- cast node :: Maybe (LHsExpr GhcRn) = ([], Direct (unLoc n) : mentions e)
    | Just (HsVar _ n) <- cast node :: Maybe (HsExpr GhcRn) = ([], [Direct (unLoc n)])
    | Just (_ :: NameSet) <- cast node = mempty
    | Just (_ :: Name) <- cast node = mempty
    | otherwise = mconcat (children (parts mentions) node)

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

bodyParts :: (LHsExpr GhcRn -> [Reference]) -> HsBind GhcRn -> ([HsBind GhcRn], [Reference])
bodyParts mentions = mconcat . children (parts mentions)

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

implicitBodies :: HsGroup GhcRn -> [LHsExpr GhcRn]
implicitBodies group = [rhs | IPBind _ _ rhs <- collectAst group]

data LocalRole = Testing | Value | Ordinary deriving Eq

data BindingRegion = BindingRegion
    { regionRole :: LocalRole, regionBinders :: [Name], regionRefs :: [Reference] }

data CheckedGroup = CheckedGroup
    { groupChecked :: TypecheckedModule, groupSource :: HsGroup GhcRn, groupIPs :: Implicits }

localBindings :: Runtime -> QuickCheck -> [TypecheckedModule] -> Locals
localBindings rt qc modules = Locals qc values (reachable aliases independent) dictionaries ipsByModule (inModule actions)
  where
    formals = implicitFormals qc modules
    groups = [CheckedGroup{groupChecked = m, groupSource = group, groupIPs = implicitParameters formals m} |
        m <- modules, Just (group, _, _, _, _) <- [tm_renamed_source m]]
    ipsByModule = Map.fromList [(checkedModule (groupChecked g), groupIPs g) | g <- groups]
    ips = map groupIPs groups
    region ip role ns (bs, refs) = BindingRegion{regionRole = role, regionBinders = ns, regionRefs = refs} : concatMap (local ip) bs
    actions = actionFields rt qc modules
    bindingRegions = concatMap topRegions groups ++ concatMap implicitRegions groups ++ concatMap actionRegions groups
    actionRegions g = concat [region ip Ordinary [] (parts (references ip) rhs) |
        rhs <- sourceActions (inModule actions (groupChecked g))]
      where ip = groupIPs g
    topRegions g = concat [region ip (if exempt b then Testing else Ordinary) [] (bodyParts (references ip) b) |
        b <- fst (parts (const []) (groupSource g))]
      where ip = groupIPs g; exempt = quickCheckBody qc (groupChecked g)
    implicitRegions g = concat [region ip (if any scalarIP vs then Value else Ordinary) (map getName vs) (parts (references ip) rhs) |
        rhs <- implicitBodies (groupSource g), let vs = [v | Just (v, _) <- [bindingAt ip (getLocA rhs)]]]
      where ip = groupIPs g
    local ip b = concat [region ip (role ns) ns body | (ns, body) <- regions]
      where
        role ns | generator b = Testing | value b ns = Value | otherwise = Ordinary
        regions = case components b of
            Just fields -> [(collectPatBinders CollNoDictBinders p, parts (references ip) e) | (p, e) <- fields]
            Nothing -> [(names b, bodyParts (references ip) b)]
    typed = map tm_typechecked_source modules
    exports = [(getName poly, getName mono) | m <- typed, ABE poly mono _ _ <- collectAst m]
    aliases = Map.fromListWith (++) ([(p, [m]) | (p, m) <- exports] ++ [(m, [p]) | (p, m) <- exports])
    valueIds = dataBinders rt aliases typed
    generator b = maybe False (isQuickCheckBinding qc) (bindingName b)
    -- A matched field has its own binders. Only an indivisible RHS must
    -- qualify all its binders together.
    value FunBind{fun_matches = matches} _ | matchGroupArity matches > 0 = False
    value b ns = not (null ns) && all (`Set.member` valueIds) ns && not (generator b)
    targets active refs = [n | Direct n <- refs] ++ [actualIP arg | AtCall arg <- refs, Set.member (formalIP arg) active]
    valueRegions = filter ((== Value) . regionRole) bindingRegions
    deps active = Map.fromList [(n, targets active (regionRefs r)) | r <- valueRegions, n <- regionBinders r]
    -- Propagate only through values. Calling or passing a named function never
    -- gives its body the caller's role. Ordinary code still checks its entire
    -- RHS; these sets only choose where to resume *inside* an exempt RHS.
    calls = Map.fromListWith (++) [(formalIP arg, [actualIP arg]) | r <- bindingRegions, AtCall arg <- regionRefs r]
    -- An unused/overridden formal cannot make its caller's IP live. Start
    -- with direct reads and propagate demand through saturated first-order calls.
    live = demand Set.empty
    testingRefs = concatMap regionRefs (filter ((== Testing) . regionRole) bindingRegions)
    demand active = let seen = reachable (deps active) (Set.fromList (targets active testingRefs))
                        next = Set.intersection (Map.keysSet calls) seen
                    in if next == active then seen else demand next
    consumed r = not (null ns) && all (`Set.member` live) ns where ns = regionBinders r
    ordinary = reachable (Map.unionWith (++) calls (deps live)) (Set.unions (passed : ordinaryRefs : map ordinaryIPs ips))
    ordinaryRefs = Set.fromList [n | r <- bindingRegions, regionRole r == Ordinary || (regionRole r == Value && not (consumed r)),
        n <- targets live (regionRefs r)]
    values = Set.fromList [n | r <- valueRegions, consumed r, all (`Set.notMember` ordinary) (regionBinders r), n <- regionBinders r]
    independent = Set.fromList (concatMap regionBinders bindingRegions) `Set.difference` values
    dictionaries = Map.fromList (concatMap declarations ips)
    -- An explicit scalar argument of an ordinary home helper crosses the
    -- same boundary as an implicit dictionary argument, including via aliases.
    helper fn = not (isQuickCheckBinding qc fn) && (isInternalName fn || maybe False (`Map.member` ipsByModule) (nameModule_maybe fn))
    passed = Set.intersection (Set.unions (map dictionaryIds ips)) (reachable (deps live) (Set.fromList
        [n | g <- groups, let ip = groupIPs g, not (Set.null (dictionaryIds ip)), e <- collectAst (groupSource g),
        let (f, args) = firstOrderCall e, Just fn <- [variable f], helper fn, arg <- args, n <- targets live (snd (parts (references ip) arg))]))

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

implicitBindingAt :: Locals -> TypecheckedModule -> SrcSpan -> Maybe (Id, LHsExpr GhcTc)
implicitBindingAt locals checked = maybe (const Nothing) bindingAt
    (Map.lookup (checkedModule checked) (moduleIPs locals))

implicitSource :: Locals -> TypecheckedModule -> HsGroup GhcRn -> [SourceBody]
implicitSource locals checked = concatMap classify . implicitBodies
  where
    binder = implicitBindingAt locals checked
    classify e | Just (v, _) <- binder (getLocA e), Set.member (getName v) (expressionValues locals) =
                   boundaries locals (fst (parts (const []) e))
               | otherwise = [Implicit e]

-- This is entered only when the existing declaration-role check has already
-- exempted the parent. Bindings without a proven value dependency are checked.
nestedSource :: Locals -> HsBind GhcRn -> [SourceBody]
nestedSource locals = boundaries locals . fst . bodyParts (const [])

boundaries :: Locals -> [HsBind GhcRn] -> [SourceBody]
boundaries locals = concatMap boundary
  where
    boundary b | Just fields <- components b, any (covered . fst) fields = concatMap field fields
               | forcedBinder b, Just n <- bindingName b, isQuickCheckBinding (quickCheckBindings locals) n =
                   nestedSource locals b
               | consumed (names b) = nestedSource locals b
               | otherwise = [Declaration b]
    consumed ns = not (null ns) && all (`Set.member` expressionValues locals) ns
    covered = consumed . collectPatBinders CollNoDictBinders
    field (p, e) | covered p = concatMap boundary (fst (parts (const []) e))
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
