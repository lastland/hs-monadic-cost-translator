-- | Method-specific certificates from finite desugared syntax. In particular,
-- pure/return are ret only at the Memorist carrier, not at Annotated/State/IO.
module ConstructionEvidence (collectEvidence, checkEvidence, methodNames) where

import ConstructionDictionary
import ConstructionAst
import ConstructionDefaults (Defaults, homeDefaults, selectsDataDefault)
import ConstructionHomeRepr
import ConstructionRoles (QuickCheck, isQuickCheckBinding)
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
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (isPredTy, splitTyConApp_maybe, splitForAllTyVars, splitFunTys)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Utils (exprType)
import GHC.Data.OrdList (fromOL)
import GHC.HsToCore.Binds (dsTcEvBinds, dsTopLHsBinds)
import GHC.HsToCore.Expr (dsLExpr)
import GHC.HsToCore.Monad (dsGetFamInstEnvs, initDs)
import GHC.Tc.Types (tcg_ev_binds, tcg_tcs)
import GHC.Tc.Types.Evidence (TcEvBinds(..))
import GHC.Types.Basic (Boxity(Boxed))
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (isDefaultMethodOcc)
import GHC.Types.Var (isId)

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
            Just (AnId identifier) | family == Computation,
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

data Site = FieldBody Name | Site String (Maybe Name) Use | MissingCertificate String | DefaultFacts Defaults

collectEvidence :: Runtime -> QuickCheck -> HomeReprs -> [C.CoreBind] -> [Name] -> TypecheckedModule -> [(SrcSpan, Name)] -> Ghc [Site]
collectEvidence rt qc homes program support checked sites = do
    selected <- methodFamilies rt
    let bindings = allBindings program
        environment = Map.fromList [(getName binder, rhs) | (binder, rhs) <- bindings]
        givens = Set.fromList ([getName var | ab <- collectAst (tm_typechecked_source checked) :: [AbsBinds], var <- abs_ev_vars ab] ++
            [getName var | (_, rhs) <- bindings, var <- coreGivens rhs])
        bindingSites env given bs = concat [record (nameSrcSpan (getName binder)) (Left (getName binder)) (uses qc selected env given rhs) |
            (binder, rhs) <- bs, not (isQuickCheckBinding qc binder), getName binder `notElem` support]
        rootSites = bindingSites environment givens (C.flattenBinds program)
    session <- getSession
    let global = fst (tm_internals_ checked)
        evidence = EvBinds (tcg_ev_binds global) : (collectAst (tm_typechecked_source checked) :: [TcEvBinds])
        typed = collectAst (tm_typechecked_source checked) :: [LHsExpr GhcTc]
    (_, result) <- liftIO $ initDs session global $ do
        dictionaries <- fmap concat (mapM (`dsTcEvBinds` pure) evidence)
        raw <- fromOL <$> dsTopLHsBinds (tm_typechecked_source checked)
        expressions <- mapM (\site -> mapM dsLExpr
            [expression | expression <- typed, typedVariable expression == Just site]) sites
        fams <- dsGetFamInstEnvs
        pure (dictionaries, (fams, raw), expressions)
    let retainedSites = case result of
            Just (dictionaries, (fams, raw), expressions) ->
                let rawBindings = allBindings (C.Rec raw : dictionaries)
                    rawEnv = Map.fromList [(getName v, rhs) | (v, rhs) <- rawBindings]
                    env = Map.union rawEnv environment
                    allGivens = Set.union givens (Set.fromList [getName v | (_, rhs) <- rawBindings, v <- coreGivens rhs])
                    -- Even an implicit instance default must be checked before
                    -- simple Core optimization can inline or erase its call.
                    rawSites = DefaultFacts (homeDefaults checked program fams rawEnv raw) : bindingSites env allGivens raw
                 in rawSites ++ concat [case [u | expression <- candidates, u@(Use actual _ _ arguments dictionaries') <- uses qc selected env allGivens expression,
                            actual == name, not (null arguments), not (null dictionaries')] of
                        [] -> [MissingCertificate (pretty location ++ ": no selected method certificate for " ++ describeName name)]
                        certificates -> record location (Right location) certificates
                    | ((location, name), candidates) <- zip sites expressions]
            Nothing -> [MissingCertificate "cannot desugar the retained source method certificates"]
    pure (rootSites ++ retainedSites)
  where
    record location site us = [FieldBody field | Left _ <- [site], Just field <- [owner]] ++ map (Site (pretty location) owner) us
      where owner = contextSite homes site

-- Collect the complete home graph before validating any caller. Source-only
-- dead uses and compiler-generated uses are obligations of the same field.
checkEvidence :: Runtime -> HomeReprs -> [TyCon] -> [Site] -> [String]
checkEvidence rt homes demands sites = concatMap check sites
  where
    inert = selectsDataDefault [facts | DefaultFacts facts <- sites]
    bodies = Map.fromSet (\field -> nubBy sameUse (Map.findWithDefault [] field observed)) declared
    observed = Map.fromListWith (++) [(field, [u]) | Site _ (Just field) u <- sites]
    declared = Set.fromList [field | FieldBody field <- sites]
    check (FieldBody _) = []
    check (DefaultFacts _) = []
    check (MissingCertificate why) = [why]
    check (Site location field u) = map ((location ++ ": ") ++) (checkUse rt homes demands bodies inert [] field u)

-- A back-edge closes only this instantiated method obligation, not the
-- enclosing list of field uses. Dictionary provenance was checked on entry.
data Active = Active Name Evidence Evidence

checkUse :: Runtime -> HomeReprs -> [TyCon] -> Map.Map Name [Use] -> (Use -> Bool) -> [Active] -> Maybe Name -> Use -> [String]
checkUse rt homes demands bodies inert stack field use@(Use name expectedClass family types dictionaries) =
    ["unapproved selected method " ++ describeName name ++ ": " ++ why ++
        " [" ++ unwords (map renderEvidence dictionaries) ++ "]" | Just why <- [verdict]]
  where
    verdict | family == ForbiddenMethod = Just "runtime observation/accounting methods have no construction permission"
            | inert use = Nothing
            | family == UnknownMethod = Just "unknown executable method/default dependency"
            | otherwise = case (types, dictionaries) of
        (ty : _, evidence : _) -> case family of
            Computation | memorist ty evidence -> Nothing
                | otherwise -> Just "requires the concrete audited Memorist instance; generic dictionaries confer no permission"
            RepresentationMethod | representationCarrier && concreteRepr ty evidence -> Nothing
                | otherwise -> Just "reprM requires a concrete method-specific construction closure and audited carrier"
            ScalarMethod | isProtected rt ty -> Just "protected thunk/annotation/accounting operand"
                | demandOperand ty -> Just "demand-data methods have no observation exemption"
                | selectedPredicate ty evidence && fieldSlot (root evidence) -> Nothing
                | (scalarType ty || maybe False scalarType (closedFamilyStep ty)) &&
                    selectedPredicate ty evidence && ordinary evidence -> Nothing
                | otherwise -> Just "unknown concrete scalar instance"
            ForbiddenMethod -> Just "runtime observation/accounting methods have no construction permission"
            UnknownMethod -> Just "unknown executable method/default dependency"
        _ -> Just "unsaturated selector has no selected instance evidence"
    root (Super _ _ parent) = root parent
    root evidence = evidence
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
        Just (tc, arguments) -> tc `elem` demands || getName tc `elem`
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
                Just obligations <- Map.lookup (getName (carrierBinder method)) bodies ->
            let active = Active (implementation method) ev carrier
            in any (identical active) stack || (all (progress active) stack &&
                all (null . checkUse rt homes demands bodies inert (active : stack) field . apply) obligations)
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
uses qc selected = walk
  where
    walk env given expression = case expression of
        C.Lam binder body -> walk env (if isId binder && isPredTy (idType binder)
            then Set.insert (getName binder) given else given) body
        C.Let binding body ->
            let bs = C.flattenBinds [binding]
                next = Map.union (Map.fromList [(getName v, rhs) | (v, rhs) <- bs]) env
            in walk next given body ++ concat [walk next given rhs | (v, rhs) <- bs, not (isQuickCheckBinding qc v)]
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
