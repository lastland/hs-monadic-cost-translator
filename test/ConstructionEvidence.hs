-- | Method-specific certificates from finite desugared syntax. In particular,
-- pure/return are ret only at the Memorist carrier, not at Annotated/State/IO.
module ConstructionEvidence (collectEvidence, checkEvidence, methodNames) where

import ConstructionDictionary
import ConstructionArithmetic (callerArithmetic)
import ConstructionActionFields (typedActions)
import ConstructionAst
import ConstructionDefaults (Defaults, homeDefaults, selectsDataDefault)
import ConstructionDemandMethods (runtimeDemandMethod, stockMethods, safeHomeType)
import ConstructionTag (tagMethod, validTag)
import ConstructionHomeRepr
import ConstructionLocals (Locals, SourceSite(..), checkedActions, components, coreLocation, implicitBindingAt, ordinaryCoreBindings, quickCheckBindings, independentBinding)
import ConstructionMethods (Methods, MethodSite(..), homeMethods, retainedMethods, defaultClass, conditionalMethod, selectedMethod)
import ConstructionRoles (QuickCheck, isQuickCheckBinding, Support, supportAt, supportBinding, sourceBinding, shrinkSupport)
import ConstructionRuntime
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.List (deleteFirstsBy, nubBy)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC hiding (exprType)
import GHC.Builtin.Types (listTyCon, maybeTyCon, tupleTyCon)
import qualified GHC.Core as C
import GHC.Core.Class (classSCSelIds, classOpItems)
import GHC.Core.FamInstEnv (FamInstEnvs)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (isPredTy, splitTyConApp_maybe, splitForAllTyVars, splitFunTys)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Utils (exprType)
import GHC.Data.Bag (unitBag)
import GHC.Data.OrdList (fromOL)
import qualified GHC.Data.Strict as Strict
import GHC.HsToCore.Binds (dsLHsBinds, dsTcEvBinds, dsTopLHsBinds)
import GHC.HsToCore.Expr (dsLExpr)
import GHC.HsToCore.Monad (dsGetFamInstEnvs, initDs)
import GHC.Tc.Types (tcg_ev_binds, tcg_tcs)
import GHC.Tc.Types.Evidence (TcEvBinds(..))
import GHC.Types.Basic (Boxity(Boxed))
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (isDefaultMethodOcc)
import GHC.Types.SrcLoc (getBufSpan)
import GHC.Types.Var (isId)
import GHC.Platform (Platform)

families :: Runtime -> Map.Map Name Family
families rt = Map.fromList
    [(name, family) | (occurrence, family) <- standardMethods,
        (label, name) <- standardNames rt, label == "Prelude." ++ occurrence]

-- The separately resolved runtime selectors are included without enumerating a
-- module's exports or assigning permission to a same-spelled local method.
methodFamilies :: Runtime -> Ghc (Map.Map Name (Family, Name))
methodFamilies rt = do
    let selected = Map.union (families rt) (Map.fromList
            ([(name, Computation) | name <- roleNames rt Primitive] ++
             [(name, RepresentationMethod) | name <- roleNames rt ReprMethod] ++
             [(name, ForbiddenMethod) | role <- [AbstractMethod, FullMethod, UsageMethod, MinimumMethod, DemandArbitraryMethod],
                 name <- roleNames rt role]))
    entries <- fmap concat $ forM (Map.toList selected) $ \(name, family) -> do
        thing <- lookupName name
        case thing of
            Just (AnId identifier) | Just cls <- isClassOpId_maybe identifier -> pure $
                (name, (family, getName cls)) : [(actual, (family, getName cls)) |
                    (method, Just (actual, _)) <- classOpItems cls, getName method == name]
            -- Overloaded wrappers are ordinary Ids. Their first argument after
            -- type abstraction is the dictionary passed at the occurrence.
            Just (AnId identifier) | family `elem` [Computation, ForbiddenMethod],
                arg : _ <- fst (splitFunTys (snd (splitForAllTyVars (idType identifier)))),
                Just (cls, [_]) <- getClassPredTys_maybe (scaledThing arg) -> pure [(name, (family, getName cls))]
            _ -> fail ("cannot bind construction method: " ++ describeName name)
    pure (Map.fromList entries)

-- Home selectors also need retained source sites: a redundant equation can
-- disappear even from raw Core. Registration asks for evidence, not permission.
methodNames :: Runtime -> [TypecheckedModule] -> Ghc [Name]
methodNames rt modules = do
    known <- methodFamilies rt
    pure (Map.keys known ++ [getName method | m <- modules, tc <- tcg_tcs (fst (tm_internals_ m)),
        Just cls <- [tyConClass_maybe tc], method <- classMethods cls])

data Site = FieldBody Name | Site String MethodSite (Maybe Name) (Maybe [Role]) Use | MissingCertificate String | DefaultFacts Defaults
    | StockFacts [(Name, Name, Type, [Use])]
    | TagFacts Platform [TyCon]
    | MethodFacts Methods

data RetainedEvidence = RetainedEvidence
    { retainedDictionaries :: [C.CoreBind], retainedFamilies :: FamInstEnvs
    , retainedBindings :: [(Id, C.CoreExpr)], methodCandidates :: [[C.CoreExpr]]
    , patternBindings :: [Maybe [(Id, C.CoreExpr)]], componentBodies :: [Maybe C.CoreExpr]
    , retainedActions :: [C.CoreExpr] }

collectEvidence :: Runtime -> Support -> Locals -> HomeReprs -> [C.CoreBind] -> [Name] -> TypecheckedModule -> [SourceSite] -> Ghc [Site]
collectEvidence rt utilities locals homes program support checked sourceSites = do
    selected <- methodFamilies rt
    let bindings = allBindings program
        environment = Map.fromList [(getName binder, rhs) | (binder, rhs) <- bindings]
        givens = Set.fromList ([getName var | ab <- collectAst (tm_typechecked_source checked) :: [AbsBinds], var <- abs_ev_vars ab] ++
            [getName var | (_, rhs) <- bindings, var <- coreGivens rhs])
        bindingSites env given bs = concat
            [let role = supportBinding utilities (getName binder)
                 collect = case role of
                     Just _ -> usesExcept qc (sourceBinding utilities)
                     Nothing -> uses qc
             in (if shrinkSupport role then [] else
                    recordRole role (coreLocation locals binder) (Left (getName binder)) (collect selected env given rhs)) ++
                 concat [recordRole (supportBinding utilities (getName v)) (coreLocation locals v)
                     (Left (getName v)) (usesExcept qc (sourceBinding utilities) selected env given body) |
                     Just _ <- [role], (v, body) <- allBindings [C.NonRec binder rhs],
                     getName v /= getName binder, sourceBinding utilities (getName v),
                     not (shrinkSupport role) || independentBinding locals (getName v), not (isQuickCheckBinding qc v)]
            | (binder, rhs) <- ordinaryCoreBindings locals bs, getName binder `notElem` support]
        rootSites = bindingSites environment givens (C.flattenBinds program)
    session <- getSession
    flags <- getSessionDynFlags
    let global = fst (tm_internals_ checked)
        evidence = EvBinds (tcg_ev_binds global) : (collectAst (tm_typechecked_source checked) :: [TcEvBinds])
        typed = atSourceSpan [(at, (name, e)) | e <- collectAst (tm_typechecked_source checked),
            Just (at, name) <- [typedVariable e]]
        sites = [(at, name) | MethodAt at name <- sourceSites]
        patterns = [at | PatternAt at <- sourceSites]
        fields = [(pat, rhs) | ComponentAt pat rhs <- sourceSites]
        actions = typedActions (checkedActions locals checked)
        bindingsByPattern = [(getLocA pat, b) | b@(L _ PatBind{pat_lhs = pat}) <-
            collectAst (tm_typechecked_source checked) :: [LHsBind GhcTc]]
        typedPatterns = atSourceSpan bindingsByPattern
        typedFields = Map.fromListWith (++) [(key, [e]) | (_, L _ b) <- bindingsByPattern,
            Just pairs <- [components b], (p, e) <- pairs, Just key <- [componentKey (getLocA p) (getLocA e)]]
        typedIP = implicitBindingAt locals checked
        requestedIPs = [(at, typedIP at) | ImplicitAt at <- sourceSites]
        missingIPs = [MissingCertificate (pretty at ++ ": no unique retained implicit parameter binding") | (at, Nothing) <- requestedIPs]
    (_, result) <- liftIO $ initDs session global $ do
        dictionaries <- fmap concat (mapM (`dsTcEvBinds` pure) evidence)
        raw <- fromOL <$> dsTopLHsBinds (tm_typechecked_source checked)
        expressions <- mapM (\(at, name) -> mapM dsLExpr
            [e | (actual, e) <- typed at, actual == name]) sites
        -- A pattern's named projections may only refer to an anonymous shared
        -- RHS. Desugar the actual local declaration, including guards and where.
        groups <- forM patterns $ \at -> case typedPatterns at of
            [b] -> Just . snd <$> dsLHsBinds (unitBag b)
            _ -> pure Nothing
        -- An unselected sibling may vanish before even raw Core is built.
        -- Desugar just the corresponding field, including implicit methods.
        fieldBodies <- forM fields $ \(pat, rhs) -> case componentKey pat rhs >>= (`Map.lookup` typedFields) of
            Just [e] -> Just <$> dsLExpr e
            _ -> pure Nothing
        actionBodies <- mapM dsLExpr actions
        -- Match the physical RHS in this module; the typed IPBind owns both
        -- the dictionary Id and its wrapped expression even in an erased equation.
        ips <- forM [pair | (_, Just pair) <- requestedIPs] $ \(v, rhs) -> (v,) <$> dsLExpr rhs
        fams <- dsGetFamInstEnvs
        pure RetainedEvidence
            { retainedDictionaries = dictionaries, retainedFamilies = fams, retainedBindings = raw ++ ips
            , methodCandidates = expressions, patternBindings = groups, componentBodies = fieldBodies
            , retainedActions = actionBodies }
    let retainedSites = case result of
            Just retained ->
                let raw = retainedBindings retained
                    rawBindings = allBindings (C.Rec raw : retainedDictionaries retained)
                    rawEnv = Map.fromList [(getName v, rhs) | (v, rhs) <- rawBindings]
                    env = Map.union rawEnv environment
                    allGivens = Set.union givens (Set.fromList [getName v | (_, rhs) <- rawBindings, v <- coreGivens rhs])
                    -- Even an implicit instance default must be checked before
                    -- simple Core optimization can inline or erase its call.
                    stockFacts = [(getName df, method, idType df, uses qc selected env allGivens rhs) |
                        (df, methods) <- stockMethods rt checked rawEnv, (method, rhs) <- methods]
                    rawSites = TagFacts (targetPlatform flags) (filter (safeHomeType rt) (tcg_tcs global)) :
                        StockFacts stockFacts : MethodFacts (homeMethods rt checked env raw (uses qc selected env allGivens)) :
                        DefaultFacts (homeDefaults checked program (retainedFamilies retained) rawEnv raw) : bindingSites env allGivens raw
                    patternSites = concat [case group of
                        Nothing -> [MissingCertificate (pretty at ++ ": no retained local pattern binding")]
                        Just bs ->
                            let localEnv = Map.union (Map.fromList [(getName v, rhs) | (v, rhs) <- allBindings [C.Rec bs]]) env
                            in record at (Right at) (concat [uses qc selected localEnv allGivens rhs | (_, rhs) <- bs])
                        | (at, group) <- zip patterns (patternBindings retained)]
                    fieldSites = concat [case field of
                        Nothing -> [MissingCertificate (pretty rhs ++ ": no retained local pattern component")]
                        Just e -> record rhs (Right rhs) (uses qc selected env allGivens e)
                        | ((_, rhs), field) <- zip fields (componentBodies retained)]
                    actionSites = concat [record at (Right at) (uses qc selected env allGivens body) |
                        (e, body) <- zip actions (retainedActions retained), let at = getLocA e]
                in rawSites ++ patternSites ++ fieldSites ++ actionSites ++ concat [case [u | expression <- candidates, u@(Use actual _ _ arguments dictionaries') <- uses qc selected env allGivens expression,
                            actual == name, not (null arguments), not (null dictionaries')] of
                        [] -> [MissingCertificate (pretty location ++ ": no selected method certificate for " ++ describeName name)]
                        certificates -> record location (Right location) certificates
                    | ((location, name), candidates) <- zip sites (methodCandidates retained)]
            Nothing -> [MissingCertificate "cannot desugar the retained source method certificates"]
    pure (rootSites ++ missingIPs ++ retainedSites)
  where
    qc = quickCheckBindings locals
    componentKey a b = case (getBufSpan a, getBufSpan b) of
        (Strict.Just p, Strict.Just e) -> Just (p, e)
        _ -> Nothing
    record location = recordRole (supportAt utilities checked location) location
    recordRole role location site us = [FieldBody field | Left _ <- [site], Just field <- [owner]] ++ map (Site (pretty location) origin owner role) us
      where
        owner = contextSite homes site
        origin = case site of
            Left name -> MethodBinding name
            Right at -> MethodDeclaration (declarationAt (independentBinding locals) checked at)

-- Collect the complete home graph before validating any caller. Source-only
-- dead uses and compiler-generated uses are obligations of the same field.
checkEvidence :: Runtime -> HomeReprs -> [TyCon] -> [Site] -> [String]
checkEvidence rt homes demands sites = concatMap check sites
  where
    checkSelected = checkUse MethodRules
        { auditedRuntime = rt, homeRepresentations = homes, demandTypes = demands
        , fieldBodies = Map.fromSet (\field -> nubBy sameUse (Map.findWithDefault [] field observed)) declared
        , isDataDefault = selectsDataDefault [facts | DefaultFacts facts <- sites]
        , stockFacts = concat [facts | StockFacts facts <- sites]
        , methodFacts = [retainedMethods [(origin, u) | Site _ origin _ _ u <- sites] facts |
            MethodFacts facts <- sites]
        , tagFacts = [(platform, types) | TagFacts platform types <- sites] }
    observed = Map.fromListWith (++) [(field, [u]) | Site _ _ (Just field) _ u <- sites]
    declared = Set.fromList [field | FieldBody field <- sites]
    check (FieldBody _) = []
    check (DefaultFacts _) = []
    check (StockFacts _) = []
    check (TagFacts _ _) = []
    check (MethodFacts _) = []
    check (MissingCertificate why) = [why]
    check (Site location origin field role u) = map ((location ++ ": ") ++) (checkSelected origin role [] field u)

data MethodRules = MethodRules
    { auditedRuntime :: Runtime, homeRepresentations :: HomeReprs, demandTypes :: [TyCon]
    , fieldBodies :: Map.Map Name [Use], isDataDefault :: Use -> Bool
    , stockFacts :: [(Name, Name, Type, [Use])], tagFacts :: [(Platform, [TyCon])]
    , methodFacts :: [Methods] }

-- A back-edge closes only this instantiated method obligation, not the
-- enclosing list of field uses. Dictionary provenance was checked on entry.
data Active = Active Name Evidence Evidence

checkUse :: MethodRules -> MethodSite -> Maybe [Role] -> [Active] -> Maybe Name -> Use -> [String]
checkUse rules origin utility = check
  where
    rt = auditedRuntime rules
    homes = homeRepresentations rules
    check stack field use@(Use name expectedClass family types dictionaries) =
        ["unapproved selected method " ++ describeName name ++ ": " ++ why ++
            " [" ++ unwords (map renderEvidence dictionaries) ++ "]" | Just why <- [verdict]]
      where
        verdict | family == ForbiddenMethod && utilityMethod use = Nothing
                | family == ForbiddenMethod = Just "runtime observation/accounting methods have no construction permission"
                | isDataDefault rules use = Nothing
                | tagUse use = Nothing
                | family == UnknownMethod && conditionalMethod rt (methodFacts rules) origin use = Nothing
                | family == UnknownMethod && concreteMethod [] use = Nothing
                | family == UnknownMethod = Just "unknown executable method/default dependency"
                | otherwise = case (types, dictionaries) of
            (ty : _, evidence : _) -> case family of
                Computation | memorist ty evidence -> Nothing
                    | otherwise -> Just "requires the concrete audited Memorist instance; generic dictionaries confer no permission"
                RepresentationMethod | representationCarrier && concreteRepr ty evidence -> Nothing
                    | otherwise -> Just "reprM requires a concrete method-specific construction closure and audited carrier"
                ScalarMethod | isProtected rt ty -> Just "protected thunk/annotation/accounting operand"
                    | selectedPredicate ty evidence && demandMethod [] name evidence -> Nothing
                    | demandOperand ty -> Just "demand-data methods have no observation exemption"
                    | callerArithmetic rt use -> Nothing
                    | selectedPredicate ty evidence && fieldSlot (root evidence) -> Nothing
                    | (scalarType ty || maybe False scalarType (closedFamilyStep ty)) &&
                        selectedPredicate ty evidence && ordinary evidence -> Nothing
                    | otherwise -> Just "unknown concrete scalar instance"
                ForbiddenMethod -> Just "runtime observation/accounting methods have no construction permission"
                UnknownMethod -> Just "unknown executable method/default dependency"
            _ -> Just "unsaturated selector has no selected instance evidence"
        root (Super _ _ parent) = root parent
        root (Transport _ parent) = root parent
        root evidence = evidence
        concreteMethod active obligation
            | Just (method, obligations) <- selectedMethod rt (methodFacts rules) obligation,
              method `notElem` active =
                all (closedDependency method (method : active)) obligations
            | otherwise = False
        closedDependency _ active obligation@(Use _ _ UnknownMethod _ _) = concreteMethod active obligation
        closedDependency method _ obligation = null (checkUse rules (MethodBinding method) Nothing [] Nothing obligation)
        utilityMethod (Use selected cls _ ts [ev])
            | Just roles <- utility, not (null roles), all (\role -> roleOf rt selected == Just role &&
                  role `elem` [AbstractMethod, FullMethod, UsageMethod]) roles,
              Just (actual, args) <- predicateOf ev >>= getClassPredTys_maybe,
              getName actual == cls, length args == length ts, and (zipWith eqType args ts) =
                supportEvidence ev
        utilityMethod _ = False
        supportEvidence Given{} = True
        supportEvidence (Instance _ _ _ ds) = all supportEvidence ds
        supportEvidence (Transport _ parent) = supportEvidence parent
        supportEvidence ev@Super{} = maybe False supportEvidence (projected homes ev)
        supportEvidence _ = False
        tagUse (Use selected cls _ ts [BuiltinTag predicate operand op]) =
            tagMethod selected predicate && case getClassPredTys_maybe predicate of
                Just (actual, args) -> cls == getName actual && length ts == length args &&
                    and (zipWith eqType ts args) &&
                    any (\(platform, homes') -> validTag platform homes' operand op) (tagFacts rules)
                _ -> False
        tagUse _ = False
        demandMethod active method ev@(Instance df predicate args ds)
            | Just (childMethod, childType) <- runtimeDemandMethod rt method df predicate args (mapMaybePredicate ds) =
                length ds == 1 && all (\child -> case predicateOf child >>= getClassPredTys_maybe of
                    Just (_, [operand]) -> eqType operand childType &&
                        ((stockChild childMethod child && demandMethod active childMethod child) ||
                            (scalarType operand && ordinary child))
                    _ -> False) ds
            | null args && null ds,
              [(formal, obligations)] <- [(p, obligations) | (owner, selected, p, obligations) <- stockFacts rules,
                  owner == getName df, selected == method],
              eqType formal predicate =
                any (\(selected, previous) -> selected == method && sameEvidence ev previous) active || all safe obligations
          where
            safe obligation | tagUse obligation = True
            safe (Use selected cls ScalarMethod (ty : _) [child])
                | Just (actual, [operand]) <- predicateOf child >>= getClassPredTys_maybe,
                  getName actual == cls, eqType operand ty, not (isProtected rt operand) =
                    demandMethod ((method, ev) : active) selected child ||
                        (scalarType operand && ordinary child)
            safe _ = False
        demandMethod _ _ _ = False
        stockChild method (Instance df predicate [] []) =
            any (\(owner, selected, formal, _) -> owner == getName df && selected == method &&
                eqType formal predicate) (stockFacts rules)
        stockChild _ _ = False
        mapMaybePredicate ds = [p | d <- ds, Just p <- [predicateOf d]]
        selectedPredicate ty evidence = case predicateOf evidence >>= getClassPredTys_maybe of
            Just (cls, arguments) -> getName cls == expectedClass && not (null arguments) && eqType (last arguments) ty
            _ -> False
        -- Only closed, exact standard instance heads registered by Runtime qualify.
        -- A given dictionary or a package/unit match is not a method certificate.
        ordinary evidence = case projected homes evidence of
            Just actualEvidence@(Instance owner predicate _ []) | resolvedProjection evidence actualEvidence, not (runtimeOwns rt (getName owner)),
                    Just (cls, arguments) <- boundInstance rt (getName owner),
                    Just (actual, selected) <- getClassPredTys_maybe predicate ->
                cls == getName actual && length arguments == length selected && and (zipWith eqType arguments selected)
            _ -> False
        demandOperand ty = case splitTyConApp_maybe ty of
            Just (tc, arguments) -> tc `elem` demandTypes rules || getName tc `elem`
                ([runtimeName rt "Memorist.Data.Interface.Demand" label | label <- ["Dem", "ThD", "MaybeD", "PairD"]] ++
                 [runtimeName rt "Memorist.Data.List" "ListD"]) || any demandOperand arguments
            _ -> False
        -- Only the actual reprM field’s lambda-bound carrier (or its superclass
        -- projection) is instantiated by the concrete carrier checked at calls.
        conditional ty evidence = case root evidence of
            Given binder predicate | methodCarrier homes binder, Just (_, [_, carrier]) <- getClassPredTys_maybe predicate -> eqType ty carrier
            _ -> False
        auditedCarrier ty evidence = isMemoristType rt ty && case root evidence of
            Instance owner predicate _ children' | runtimeOwns rt (getName owner),
                    Just (cls, _) <- boundInstance rt (getName owner),
                    Just (actual, arguments) <- getClassPredTys_maybe predicate ->
                cls == getName actual && not (null arguments) && eqType (last arguments) ty && all ordinary children'
            _ -> False
        memorist ty evidence = selectedPredicate ty evidence && (conditional ty evidence || auditedCarrier ty evidence)
        -- Neither a home nor an audited runtime reprM grants its abstract field.
        concreteRepr ty evidence = case evidence of
            _ | fieldSlot (root evidence), Just (cls, [argument]) <- predicateOf evidence >>= getClassPredTys_maybe ->
                getName cls == reprClass && eqType argument ty
            Super{} | Just actual@Instance{} <- projected homes evidence -> concreteRepr ty actual
            Transport{} | Just actual@Instance{} <- projected homes evidence -> concreteRepr ty actual
            Instance owner predicate instantiation dependencies | Just (method, context) <- selectedHome homes field (getName owner) instantiation ty,
                    Just (cls, [argument]) <- getClassPredTys_maybe predicate ->
                getName cls == reprClass && eqType argument ty && length context == length dependencies &&
                    and (zipWith contextChild context dependencies) && replay method instantiation dependencies evidence
            Instance owner predicate _ dependencies | runtimeOwns rt (getName owner),
                    Just (cls, _) <- boundInstance rt (getName owner),
                    cls == runtimeName rt "Memorist.Data.Interface.Data" "Repr",
                    Just (actual, [argument]) <- getClassPredTys_maybe predicate,
                    getName actual == cls, eqType argument ty -> case splitTyConApp_maybe ty of
                _ | scalarType ty -> null dependencies
                Just (tc, arguments) | tc `elem` [maybeTyCon, listTyCon, tupleTyCon Boxed 2] ->
                    length dependencies == length arguments && and (zipWith concreteRepr arguments dependencies)
                _ -> False
            _ -> False
        reprClass = runtimeName rt "Memorist.Data.Interface.Data" "Repr"
        -- Contexts are arguments, not method calls. Check their exact predicates
        -- and provenance here; replay decides which fields are executed.
        contextChild predicate child = case predicateOf child of
            Just actual -> eqType actual predicate && resolved child
            _ -> False
        -- Replay even inside an uncalled method: substitute the supplied values,
        -- keeping only the enclosing definition's own slots conditional.
        replay method args ds ev = case dictionaries of
            [_, carrier] | Just apply <- instantiateUse method args ds carrier,
                    Just obligations <- Map.lookup (getName (carrierBinder method)) (fieldBodies rules) ->
                let active = Active (implementation method) ev carrier
                in any (identical active) stack || (all (progress active) stack &&
                    all (null . check (active : stack) field . apply) obligations)
            _ -> False
        identical (Active method ev carrier) (Active other previous oldCarrier) =
            method == other && sameEvidence ev previous && sameEvidence carrier oldCarrier
        progress (Active _ ev@(Instance owner _ args ds) carrier) (Active _ ancestor@(Instance previous _ oldArgs oldDs) oldCarrier) =
            owner /= previous || descendant ev ancestor ||
                (sameEvidence carrier oldCarrier && permutation eqType args oldArgs && permutation sameEvidence ds oldDs) ||
                (sameEvidence ev ancestor && specializes (root oldCarrier) (root carrier))
        progress _ _ = False
        -- Reordering existing arguments and context dictionaries gives a finite
        -- set of obligations for this DFun and carrier. It permits another replay;
        -- only identical (ordered) evidence above can actually close the edge.
        permutation eq xs ys = length xs == length ys && null (deleteFirstsBy eq xs ys)
        -- The DFun is unchanged, but switching from the method parameter to the
        -- audited concrete carrier opens a new obligation; it is not a back-edge.
        specializes Given{} Instance{} = True
        specializes _ _ = False
        -- An inert dictionary confers no method permission, even at a source
        -- site erased from Core. It must still have a fully selected origin.
        resolved (Instance _ _ _ children') = all resolved children'
        resolved (Transport _ parent) = resolved parent
        resolved ev@Super{}
            | fieldSlot (root ev) = True
            | Just actual@Instance{} <- projected homes ev = resolvedProjection ev actual
            | otherwise = False
        resolved evidence = fieldSlot evidence
        resolvedProjection (Super _ _ parent) actual = resolved parent && resolved actual
        resolvedProjection _ actual = resolved actual
        fieldSlot (Given binder _) = methodContext homes field binder
        fieldSlot _ = False
        representationCarrier = case (types, dictionaries) of
            ([_, _, carrier], [_, evidence]) -> case predicateOf evidence >>= getClassPredTys_maybe of
                Just (cls, _) | getName cls == runtimeName rt "Memorist.Base.Core" "Memorist" ->
                    -- The second dictionary belongs to reprM's carrier, not Repr.
                    conditional carrier evidence || auditedCarrier carrier evidence
                _ -> False
            _ -> False

uses :: QuickCheck -> Map.Map Name (Family, Name) -> Environment -> Set.Set Name -> C.CoreExpr -> [Use]
uses qc = usesExcept qc (const False)

usesExcept :: QuickCheck -> (Name -> Bool) -> Map.Map Name (Family, Name) -> Environment -> Set.Set Name -> C.CoreExpr -> [Use]
usesExcept qc boundary selected = walk
  where
    walk env given expression = case expression of
        C.Lam binder body -> walk env (if isId binder && isPredTy (idType binder)
            then Set.insert (getName binder) given else given) body
        C.Let binding body ->
            let bs = C.flattenBinds [binding]
                next = Map.union (Map.fromList [(getName v, rhs) | (v, rhs) <- bs]) env
            in walk next given body ++ concat [walk next given rhs | (v, rhs) <- bs, not (isQuickCheckBinding qc v), not (boundary (getName v))]
        C.Cast body _ -> walk env given body
        C.Tick _ body -> walk env given body
        C.Case scrutinee _ _ alternatives -> walk env given scrutinee ++ concat [walk env given rhs | C.Alt _ _ rhs <- alternatives]
        _ -> case C.collectArgs expression of
            (C.Var identifier, arguments) | Just (family, cls) <- selectedFamily identifier ->
                Use (getName identifier) cls family [ty | C.Type ty <- arguments]
                    [trace env given Set.empty argument | argument <- arguments,
                        not (C.isTyCoArg argument), isPredTy (exprType argument)] :
                    concat [walk env given argument | argument <- arguments,
                        not (C.isTyCoArg argument), not (isPredTy (exprType argument))]
            _ -> case expression of
                C.App function argument -> walk env given function ++ (if C.isTyCoArg argument then [] else walk env given argument)
                _ -> []
    selectedFamily identifier = case Map.lookup (getName identifier) selected of
        Just family -> Just family
        Nothing | Just cls <- defaultClass identifier -> Just (UnknownMethod, cls)
        Nothing | isDefaultMethodOcc (nameOccName (getName identifier)) -> Just (UnknownMethod, getName identifier)
        Nothing | Just cls <- isClassOpId_maybe identifier, identifier `notElem` classSCSelIds cls -> Just (UnknownMethod, getName cls)
        _ -> Nothing
coreGivens :: C.CoreExpr -> [Id]
coreGivens expression = case expression of
    C.Lam binder body -> [binder | isId binder, isPredTy (idType binder)] ++ coreGivens body
    C.App f x -> coreGivens f ++ coreGivens x
    C.Let binding body -> concatMap coreGivens (C.rhssOfBind binding) ++ coreGivens body
    C.Case scrutinee _ _ alternatives -> coreGivens scrutinee ++ concat [coreGivens rhs | C.Alt _ _ rhs <- alternatives]
    C.Cast body _ -> coreGivens body
    C.Tick _ body -> coreGivens body
    _ -> []
