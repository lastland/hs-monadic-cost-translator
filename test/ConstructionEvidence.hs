-- | Method-specific certificates from finite desugared syntax. In particular,
-- pure/return are ret only at the Memorist carrier, not at Annotated/State/IO.
module ConstructionEvidence (checkEvidence, methodNames) where

import ConstructionAst
import ConstructionRuntime
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC hiding (exprType)
import GHC.Builtin.Types (listTyCon, maybeTyCon, tupleTyCon)
import qualified GHC.Core as C
import GHC.Core.Class (classSCSelIds, classOpItems)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (isPredTy, splitTyConApp_maybe)
import GHC.Core.Utils (exprType)
import GHC.Data.OrdList (fromOL)
import GHC.HsToCore.Binds (dsTcEvBinds, dsTopLHsBinds)
import GHC.HsToCore.Expr (dsLExpr)
import GHC.HsToCore.Monad (initDs)
import GHC.Tc.Types (tcg_ev_binds)
import GHC.Tc.Types.Evidence (TcEvBinds(..))
import GHC.Types.Basic (Boxity(Boxed))
import GHC.Types.Id (isDFunId)
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (isDefaultMethodOcc)
import GHC.Types.Var (isId)
import GHC.Unit.Module.ModGuts (mg_binds)

data Family = Computation | RepresentationMethod | ScalarMethod | ForbiddenMethod | UnknownMethod deriving (Eq)
data Evidence = Given Type | Instance Name Type [Evidence] | Super Type Evidence | Unknown String
data Use = Use Name Name Family [Type] [Evidence]
type Environment = Map.Map Name C.CoreExpr

families :: Runtime -> Map.Map Name Family
families rt = Map.fromList
    ([(name, Computation) | (label, name) <- standardNames rt, label `elem` map ("Prelude." ++)
        ["pure", "return", ">>=", ">>", "=<<", "fmap", "<$>", "<*>", "<*", "*>"]] ++
     [(name, ScalarMethod) | (label, name) <- standardNames rt, label `elem` map ("Prelude." ++)
        ["+", "-", "*", "negate", "abs", "signum", "fromInteger", "fromRational", "/", "recip",
         "div", "mod", "quot", "rem", "divMod", "quotRem", "==", "/=", "compare", "<", "<=", ">", ">=", "max", "min",
         "show", "showsPrec", "showList"]])

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
        pure ([(name, (family, getName cls)) | Just (AnId identifier) <- [thing], Just cls <- [isClassOpId_maybe identifier]] ++
            [(actual, (family, getName cls)) | Just (AnId identifier) <- [thing], Just cls <- [isClassOpId_maybe identifier],
                (method, Just (actual, _)) <- classOpItems cls, getName method == name])
    pure (Map.fromList entries)

methodNames :: Runtime -> Ghc [Name]
methodNames rt = Map.keys <$> methodFamilies rt

checkEvidence :: Runtime -> [TyCon] -> [Name] -> TypecheckedModule -> [(SrcSpan, Name)] -> Ghc [String]
checkEvidence rt demands support checked sites = do
    selected <- methodFamilies rt
    desugared <- desugarModule checked
    let program = mg_binds (coreModule desugared)
        bindings = concatMap allBindings program
        environment = Map.fromList [(getName binder, rhs) | (binder, rhs) <- bindings]
        givens = Set.fromList ([getName var | ab <- collectAst (tm_typechecked_source checked) :: [AbsBinds], var <- abs_ev_vars ab] ++
            [getName var | (_, rhs) <- bindings, var <- coreGivens rhs])
        rootProblems = concat [concatMap (checkUse rt demands) (uses selected environment givens rhs) |
            (binder, rhs) <- concatMap pairs program, getName binder `notElem` support]
    session <- getSession
    let global = fst (tm_internals_ checked)
        evidence = EvBinds (tcg_ev_binds global) : (collectAst (tm_typechecked_source checked) :: [TcEvBinds])
        typed = collectAst (tm_typechecked_source checked) :: [LHsExpr GhcTc]
    (_, result) <- liftIO $ initDs session global $ do
        dictionaries <- fmap concat (mapM (`dsTcEvBinds` pure) evidence)
        raw <- fromOL <$> dsTopLHsBinds (tm_typechecked_source checked)
        expressions <- mapM (\site -> mapM dsLExpr
            [expression | expression <- typed, typedMethodHead expression == Just site]) sites
        pure (dictionaries, raw, expressions)
    let siteProblems = case result of
            Just (dictionaries, raw, expressions) ->
                let rawBindings = concatMap allBindings (C.Rec raw : dictionaries)
                    env = Map.union (Map.fromList [(getName v, rhs) | (v, rhs) <- rawBindings]) environment
                    allGivens = Set.union givens (Set.fromList [getName v | (_, rhs) <- rawBindings, v <- coreGivens rhs])
                    -- Even an implicit instance default must be checked before
                    -- simple Core optimization can inline or erase its call.
                    rawProblems = concat [problems (uses selected env allGivens rhs) |
                        (binder, rhs) <- raw, getName binder `notElem` support]
                 in rawProblems ++ concat [case [u | expression <- candidates, u@(Use actual _ _ arguments dictionaries') <- uses selected env allGivens expression,
                            actual == name, not (null arguments), not (null dictionaries')] of
                        [] -> [pretty location ++ ": no selected method certificate for " ++ describeName name]
                        certificates -> [pretty location ++ ": " ++ problem | problem <- problems certificates]
                    | ((location, name), candidates) <- zip sites expressions]
            Nothing -> ["cannot desugar the retained source method certificates"]
    pure (rootProblems ++ siteProblems)
  where
    problems = concatMap (checkUse rt demands)

-- Explicit type application retains its dictionary on a typed prefix, not on
-- the bare HsVar. Follow only that prefix and compiler wrappers back to the same
-- located method Id. Never borrow evidence from a surrounding term/application.
typedMethodHead :: LHsExpr GhcTc -> Maybe (SrcSpan, Name)
typedMethodHead expression = at (getLocA expression) (unLoc expression)
  where
    at location (HsVar _ name) = Just (location, getName (unLoc name))
    at _ (HsAppType _ function _) = typedMethodHead function
    at _ (HsPar _ function) = typedMethodHead function
    at _ (ExprWithTySig _ function _) = typedMethodHead function
    at location (XExpr (WrapExpr (HsWrap _ function))) = at location function
    at _ _ = Nothing

checkUse :: Runtime -> [TyCon] -> Use -> [String]
checkUse rt demands (Use name expectedClass family types dictionaries) =
    ["unapproved selected method " ++ describeName name ++ ": " ++ why ++
        " [" ++ unwords (map renderEvidence dictionaries) ++ "]" | Just why <- [verdict]]
  where
    verdict | family == ForbiddenMethod = Just "runtime observation/accounting methods have no construction permission"
            | family == UnknownMethod = Just "unknown executable method/default dependency"
            | otherwise = case (types, dictionaries) of
        (ty : _, evidence : _) -> case family of
            Computation | memorist ty evidence -> Nothing
                | otherwise -> Just "requires the concrete audited Memorist instance; generic dictionaries confer no permission"
            RepresentationMethod | concreteRepr ty evidence && representationCarrier -> Nothing
                | otherwise -> Just "reprM requires a concrete method-specific construction closure and audited carrier"
            ScalarMethod | isProtected rt ty -> Just "protected thunk/annotation/accounting operand"
                | demandOperand ty -> Just "demand-data methods have no observation exemption"
                | scalarType ty && selectedPredicate ty evidence && ordinary evidence -> Nothing
                | otherwise -> Just "unknown concrete scalar instance"
            ForbiddenMethod -> Just "runtime observation/accounting methods have no construction permission"
            UnknownMethod -> Just "unknown executable method/default dependency"
        _ -> Just "unsaturated selector has no selected instance evidence"
    root (Super _ parent) = root parent
    root evidence = evidence
    predicateOf (Super predicate _) = Just predicate
    predicateOf (Given predicate) = Just predicate
    predicateOf (Instance _ predicate _) = Just predicate
    predicateOf Unknown{} = Nothing
    selectedPredicate ty evidence = case predicateOf evidence >>= getClassPredTys_maybe of
        Just (cls, arguments) -> getName cls == expectedClass && not (null arguments) && eqType (last arguments) ty
        _ -> False
    -- Only closed, exact standard instance heads registered by Runtime qualify.
    -- A given dictionary or a package/unit match is not a method certificate.
    ordinary evidence = case root evidence of
        Instance owner predicate [] | not (runtimeOwns rt owner),
                Just (cls, arguments) <- boundInstance rt owner,
                Just (actual, selected) <- getClassPredTys_maybe predicate ->
            cls == getName actual && length arguments == length selected && and (zipWith eqType arguments selected)
        _ -> False
    demandOperand ty = case splitTyConApp_maybe ty of
        Just (tc, arguments) -> tc `elem` demands || getName tc `elem`
            ([runtimeName rt "Memorist.Data.Interface.Demand" label | label <- ["Dem", "ThD", "MaybeD", "PairD"]] ++
             [runtimeName rt "Memorist.Data.List" "ListD"]) || any demandOperand arguments
        _ -> False
    memorist ty evidence
        | isMemoristType rt ty, selectedPredicate ty evidence = case root evidence of
            Instance owner predicate children' | runtimeOwns rt owner -> case (boundInstance rt owner, getClassPredTys_maybe predicate) of
                (Just (cls, _), Just (selected, arguments)) -> cls == getName selected &&
                    not (null arguments) && eqType (last arguments) ty && all ordinary children'
                _ -> False
            _ -> False
        | otherwise = False
    -- These pinned runtime reprM bodies use return/lazy/recursive reprM only.
    -- Their abstract fields are neither selected nor granted by this closure.
    concreteRepr ty evidence = case root evidence of
        Instance owner predicate dependencies | runtimeOwns rt owner,
                Just (cls, _) <- boundInstance rt owner,
                cls == runtimeName rt "Memorist.Data.Interface.Data" "Repr",
                Just (actual, [argument]) <- getClassPredTys_maybe predicate,
                getName actual == cls, eqType argument ty -> case splitTyConApp_maybe ty of
            _ | scalarType ty -> null dependencies
            Just (tc, arguments) | tc `elem` [maybeTyCon, listTyCon, tupleTyCon Boxed 2] ->
                length dependencies == length arguments && and (zipWith concreteRepr arguments dependencies)
            _ -> False
        _ -> False
    representationCarrier = case (types, dictionaries) of
        ([_, _, carrier], [_, evidence]) -> case predicateOf evidence >>= getClassPredTys_maybe of
            Just (cls, _) | getName cls == runtimeName rt "Memorist.Base.Core" "Memorist" ->
                -- The second dictionary belongs to reprM's carrier, not Repr.
                isMemoristType rt carrier && case root evidence of
                    Instance owner predicate children' | runtimeOwns rt owner,
                            Just (boundClass, _) <- boundInstance rt owner,
                            Just (actual, arguments) <- getClassPredTys_maybe predicate ->
                        boundClass == getName actual && not (null arguments) && eqType (last arguments) carrier && all ordinary children'
                    _ -> False
            _ -> False
        _ -> False

uses :: Map.Map Name (Family, Name) -> Environment -> Set.Set Name -> C.CoreExpr -> [Use]
uses selected = walk
  where
    walk env given expression = case expression of
        C.Lam binder body -> walk env (if isId binder && isPredTy (idType binder)
            then Set.insert (getName binder) given else given) body
        C.Let binding body ->
            let bs = pairs binding
                next = Map.union (Map.fromList [(getName v, rhs) | (v, rhs) <- bs]) env
            in walk next given body ++ concatMap (walk next given . snd) bs
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
    trace env given seen expression = case expression of
        C.Tick _ body -> trace env given seen body
        C.Cast{} -> Unknown "coerced dictionary"
        _ -> case C.collectArgs expression of
            (C.Var identifier, arguments)
                | isDFunId identifier -> Instance (getName identifier) (exprType expression)
                    [trace env given seen arg | arg <- arguments, not (C.isTyCoArg arg), isPredTy (exprType arg)]
                | null arguments, getName identifier `Set.member` given -> Given (idType identifier)
                | null arguments, getName identifier `Set.notMember` seen, Just rhs <- Map.lookup (getName identifier) env ->
                    trace env given (Set.insert (getName identifier) seen) rhs
                | Just cls <- isClassOpId_maybe identifier, identifier `elem` classSCSelIds cls,
                  [arg] <- [a | a <- arguments, not (C.isTyCoArg a)] -> Super (exprType expression) (trace env given seen arg)
            _ -> Unknown (pretty expression)

renderEvidence :: Evidence -> String
renderEvidence (Given ty) = "given " ++ pretty ty
renderEvidence (Instance name ty dependencies) = describeName name ++ " :: " ++ pretty ty ++
    concatMap ((" {" ++) . (++ "}") . renderEvidence) dependencies
renderEvidence (Super ty parent) = "super " ++ pretty ty ++ " of " ++ renderEvidence parent
renderEvidence (Unknown why) = "unresolved " ++ why

coreGivens :: C.CoreExpr -> [Id]
coreGivens expression = case expression of
    C.Lam binder body -> [binder | isId binder, isPredTy (idType binder)] ++ coreGivens body
    C.App f x -> coreGivens f ++ coreGivens x
    C.Let binding body -> concatMap (coreGivens . snd) (pairs binding) ++ coreGivens body
    C.Case scrutinee _ _ alternatives -> coreGivens scrutinee ++ concat [coreGivens rhs | C.Alt _ _ rhs <- alternatives]
    C.Cast body _ -> coreGivens body
    C.Tick _ body -> coreGivens body
    _ -> []

pairs :: C.CoreBind -> [(Id, C.CoreExpr)]
pairs (C.NonRec binder rhs) = [(binder, rhs)]
pairs (C.Rec bindings) = bindings

allBindings :: C.CoreBind -> [(Id, C.CoreExpr)]
allBindings binding = concat [(binder, rhs) : inside rhs | (binder, rhs) <- pairs binding]
  where
    inside (C.Let b body) = allBindings b ++ inside body
    inside (C.App f x) = inside f ++ inside x
    inside (C.Lam _ body) = inside body
    inside (C.Case scrutinee _ _ alternatives) = inside scrutinee ++ concat [inside rhs | C.Alt _ _ rhs <- alternatives]
    inside (C.Cast body _) = inside body
    inside (C.Tick _ body) = inside body
    inside _ = []
