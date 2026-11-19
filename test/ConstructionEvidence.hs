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
import GHC.Builtin.Names (dataToTagClassName)
import GHC.Builtin.PrimOps (PrimOp(..))
import qualified GHC.Core as C
import GHC.Core.Class (classSCSelIds, classOpItems, classTyCon)
import GHC.Core.Coercion (Role(Representational), coercionRKind, eqCoercion, mkSymCo, mkUnbranchedAxInstCo)
import GHC.Core.DataCon (dataConFullSig, dataConOrigArgTys, dataConInstOrigArgTys)
import GHC.Core.FamInstEnv (FamInst(..))
import GHC.Core.InstEnv (ClsInst(..))
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep (Coercion, scaledThing)
import GHC.Core.TyCon (isAlgTyCon, newTyConCo)
import GHC.Core.Type (isPredTy, splitTyConApp_maybe, mkTyConTy)
import GHC.Core.Utils (exprType)
import GHC.Data.OrdList (fromOL)
import GHC.HsToCore.Binds (dsTcEvBinds, dsTopLHsBinds)
import GHC.HsToCore.Expr (dsLExpr)
import GHC.HsToCore.Monad (initDs)
import GHC.Tc.Types (tcg_ev_binds, tcg_fam_insts, tcg_insts, tcg_tcs)
import GHC.Tc.Types.Evidence (TcEvBinds(..))
import GHC.Types.Id (isDFunId, isDataConId_maybe, isPrimOpId_maybe)
import GHC.Types.Name (nameModule_maybe, nameOccName)
import GHC.Types.Name.Occurrence (isDefaultMethodOcc)
import GHC.Types.Var (isId)
import GHC.Unit.Module.ModGuts (mg_binds)

data Family = Computation | RepresentationMethod | ScalarMethod | EmptyUsageDefault | ConstructorTagMethod | UnknownMethod deriving (Eq)
data Evidence = Given Type | Instance Name Type [Evidence] | Super Type Evidence | CompilerTag Type Type | Unknown String
data Use = Use Name Family [Type] [Evidence]
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
methodFamilies :: Runtime -> Ghc (Map.Map Name Family)
methodFamilies rt = do
    let selected = Map.union (families rt) (Map.fromList
            ([(name, Computation) | name <- roleNames rt Primitive] ++
             [(name, RepresentationMethod) | name <- roleNames rt ReprMethod]))
    defaults <- fmap concat $ forM (Map.toList selected ++ [(name, EmptyUsageDefault) | name <- roleNames rt UsageMethod]) $ \(name, family) -> do
        thing <- lookupName name
        pure [(actual, family) | Just (AnId identifier) <- [thing], Just cls <- [isClassOpId_maybe identifier],
            (method, Just (actual, _)) <- classOpItems cls, getName method == name]
    tag <- lookupName dataToTagClassName
    let tags = [(getName method, ConstructorTagMethod) | Just (ATyCon tc) <- [tag],
            Just cls <- [tyConClass_maybe tc], [method] <- [classMethods cls]]
    pure (Map.unions [selected, Map.fromList defaults, Map.fromList tags])

methodNames :: Runtime -> Ghc [Name]
methodNames rt = Map.keys <$> methodFamilies rt

checkEvidence :: Runtime -> [Module] -> [TyCon] -> [Name] -> TypecheckedModule -> [(SrcSpan, Name)] -> Ghc [String]
checkEvidence rt homes demands support checked sites = do
    selected <- methodFamilies rt
    desugared <- desugarModule checked
    let program = mg_binds (coreModule desugared)
        bindings = concatMap allBindings program
        environment = Map.fromList [(getName binder, rhs) | (binder, rhs) <- bindings]
        givens = Set.fromList ([getName var | ab <- collectAst (tm_typechecked_source checked) :: [AbsBinds], var <- abs_ev_vars ab] ++
            [getName var | (_, rhs) <- bindings, var <- coreGivens rhs])
    derived <- derivedDemandMethods rt checked environment
    let rootProblems = concat [concatMap (checkUse rt homes demands (lookup (getName binder) derived)) (uses selected environment givens rhs) |
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
                in concat [case [u | expression <- candidates, u@(Use actual _ arguments dictionaries') <- uses selected env allGivens expression,
                            actual == name, not (null arguments), not (null dictionaries')] of
                        [] -> [pretty location ++ ": no selected method certificate for " ++ describeName name]
                        certificates -> [pretty location ++ ": " ++ problem | problem <- problems certificates]
                    | ((location, name), candidates) <- zip sites expressions]
            Nothing -> ["cannot desugar the retained source method certificates"]
    pure (rootProblems ++ siteProblems)
  where
    problems = concatMap (checkUse rt homes demands Nothing)

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

-- Stock deriving on a concrete, checked demand datatype is a declaration role.
-- Bind that role through the actual instance head, dictionary constructor and
-- typed method fields, never logical source spans or $c names. Selected field
-- evidence is still checked below rather than skipping the derived body.
derivedDemandMethods :: Runtime -> TypecheckedModule -> Environment -> Ghc [(Name, Type)]
derivedDemandMethods rt checked environment = do
    classes <- fmap concat $ forM ["Prelude.==", "Prelude.showsPrec"] $ \label -> case lookup label (standardNames rt) of
        Just name -> do
            thing <- lookupName name
            pure [getName cls | Just (AnId identifier) <- [thing], Just cls <- [isClassOpId_maybe identifier]]
        Nothing -> pure []
    declarations <- case tm_renamed_source checked of
        Nothing -> pure []
        Just (group, _, _, _, _) -> fmap concat $ forM (collectAst group :: [LDerivDecl GhcRn]) $ \declaration ->
            case unLoc declaration of
                DerivDecl { deriv_type = signature, deriv_strategy = strategy, deriv_overlap_mode = Nothing }
                    | stock strategy,
                      ([], context, headType) <- splitLHsInstDeclTy (dropWildCards signature),
                      maybe True (null . unLoc) context,
                      HsAppTy _ (L _ (HsTyVar _ NotPromoted cls)) (L _ (HsTyVar _ NotPromoted target)) <- unLoc headType,
                      unLoc cls `elem` classes -> do
                        thing <- lookupName (unLoc target)
                        pure [(unLoc cls, tc) | Just (ATyCon tc) <- [thing], concreteDemand tc]
                _ -> pure []
    pure (concat [dictionaryMethods instance' target | (cls, target) <- declarations,
        instance' <- tcg_insts global, getName (is_cls instance') == cls, null (is_tvs instance'),
        [ty] <- [is_tys instance'], eqType ty (mkTyConTy target)])
  where
    global = fst (tm_internals_ checked)
    stock Nothing = True
    stock (Just (L _ StockStrategy{})) = True
    stock _ = False
    concreteDemand tc = tc `elem` tcg_tcs global && isAlgTyCon tc && not (isNewTyCon tc) && tyConArity tc == 0 &&
        any (\equation -> fi_fam equation == runtimeName rt "Memorist.Data.Interface.Demand" "Dem" &&
            case splitTyConApp_maybe (fi_rhs equation) of Just (actual, _) -> actual == tc; _ -> False) (tcg_fam_insts global) &&
        all fields (tyConDataCons tc)
    fields constructor = case dataConFullSig constructor of
        (_, [], [], [], _, _) -> all (thunkDemand . scaledThing) (dataConOrigArgTys constructor)
        _ -> False
    thunkDemand ty = case splitTyConApp_maybe ty of
        Just (tc, [payload]) -> getName tc == runtimeName rt "Memorist.Data.Interface.Demand" "ThD" && scalarType payload
        _ -> False
    dictionaryMethods instance' target = case Map.lookup (is_dfun_name instance') environment of
        Just rhs | (C.Var constructor, C.Type argument : fields') <- C.collectArgs (dictionaryBody Set.empty rhs),
                [actual] <- tyConDataCons (classTyCon (is_cls instance')),
                isDataConId_maybe constructor == Just actual, eqType argument (mkTyConTy target),
                let methods = [method | C.Var method <- fields'],
                length methods == length fields',
                let expected = map scaledThing (dataConInstOrigArgTys actual [argument]),
                length expected == length methods,
                and (zipWith (eqType . idType) methods expected) -> [(getName method, argument) | method <- methods]
        _ -> []
    dictionaryBody seen expression = case expression of
        C.Var v | getName v `Set.notMember` seen, Just rhs <- Map.lookup (getName v) environment ->
            dictionaryBody (Set.insert (getName v) seen) rhs
        C.Let _ body -> dictionaryBody seen body
        C.Tick _ body -> dictionaryBody seen body
        _ -> expression

checkUse :: Runtime -> [Module] -> [TyCon] -> Maybe Type -> Use -> [String]
checkUse rt homes demands observation (Use name family types dictionaries) =
    ["unapproved selected method " ++ describeName name ++ ": " ++ why ++
        " [" ++ unwords (map renderEvidence dictionaries) ++ "]" | Just why <- [verdict]]
  where
    verdict | family == ConstructorTagMethod = case (observation, types, dictionaries) of
            (Just target, [level, ty], [CompilerTag actualLevel actualTy])
                | eqType ty target, eqType ty actualTy, eqType level actualLevel -> Nothing
            _ -> Just "constructor tags require the exact compiler dictionary in a checked stock-derived demand method"
            | family == UnknownMethod =
        if maybe False (`elem` homes) (nameModule_maybe name) &&
            case dictionaries of evidence : _ -> lexical evidence || safeHome evidence; _ -> False
        then Nothing else Just "unknown executable method/default dependency"
            | otherwise = case (types, dictionaries) of
        (ty : _, evidence : _) -> case family of
            Computation | memorist ty evidence -> Nothing
                | otherwise -> Just "requires the audited Memorist instance or a lexical Memorist/MonadMemorist contract"
            RepresentationMethod | safeRepr evidence && representationCarrier -> Nothing
                | otherwise -> Just "reprM requires a checked local, audited runtime, or lexical Repr method contract"
            ScalarMethod | isProtected rt ty -> Just "protected thunk/annotation/accounting operand"
                | Nothing <- observation, demandOperand ty -> Just "demand-data methods require a checked observation declaration"
                | Just _ <- observation, demandScalar ty evidence -> Nothing
                | scalarType ty && ordinary evidence -> Nothing
                | lexical evidence || safeHome evidence -> Nothing
                | otherwise -> Just "unknown concrete scalar instance"
            EmptyUsageDefault | demandPredicate evidence -> Nothing
                | otherwise -> Just "audited empty-usage default requires its Demand dictionary"
            ConstructorTagMethod -> Just "unchecked constructor tag method"
            UnknownMethod -> Just "unknown executable method/default dependency"
        _ -> Just "unsaturated selector has no selected instance evidence"
    root (Super _ parent) = root parent
    root evidence = evidence
    lexical evidence = case root evidence of Given _ -> True; _ -> False
    home evidence = case root evidence of
        Instance owner _ _ -> maybe False (`elem` homes) (nameModule_maybe owner)
        _ -> False
    safeHome evidence = case root evidence of
        Instance _ _ dependencies -> home evidence && all constructionEvidence dependencies
        _ -> False
    constructionEvidence evidence = case root evidence of
        Given _ -> True
        Instance owner predicate dependencies
            | reprPredicate predicate -> safeRepr evidence
            | home evidence -> all constructionEvidence dependencies
            | Just (cls, _) <- boundInstance rt owner,
              cls `elem` [runtimeName rt "Memorist.Data.Interface.Demand" label | label <- ["Demand", "LBoundDemand", "DemandArb"]] ->
                all constructionEvidence dependencies
            | otherwise -> ordinary evidence
        _ -> False
    ordinary evidence = case root evidence of
        Given _ -> True
        Instance owner _ children' ->
            maybe False (\actual -> moduleUnit actual `elem`
                [moduleUnit m | (_, n) <- standardNames rt, Just m <- [nameModule_maybe n]]) (nameModule_maybe owner) &&
                all ordinary children'
        _ -> False
    demandScalar ty evidence
        | name `notElem` [actual | (label, actual) <- standardNames rt,
            label `elem` map ("Prelude." ++) ["==", "/=", "show", "showsPrec", "showList"]] = False
        | Just (tc, [payload]) <- splitTyConApp_maybe ty,
          getName tc == runtimeName rt "Memorist.Data.Interface.Demand" "ThD", scalarType payload = case root evidence of
            Instance owner predicate [child] | Just (cls, _) <- boundInstance rt owner,
                    Just (selected, [argument]) <- getClassPredTys_maybe predicate,
                    cls == getName selected, eqType argument ty,
                    Just (childClass, [childArgument]) <- evidencePredicate child ->
                getName childClass == cls && eqType childArgument payload && ordinary child
            _ -> False
        | otherwise = False
    evidencePredicate evidence = case root evidence of
        Instance _ predicate _ -> getClassPredTys_maybe predicate
        _ -> Nothing
    demandOperand ty = case splitTyConApp_maybe ty of
        Just (tc, arguments) -> tc `elem` demands || getName tc `elem`
            ([runtimeName rt "Memorist.Data.Interface.Demand" label | label <- ["Dem", "ThD", "MaybeD", "PairD"]] ++
             [runtimeName rt "Memorist.Data.List" "ListD"]) || any demandOperand arguments
        _ -> False
    memorist ty evidence
        | isMemoristType rt ty = case root evidence of
            Instance owner predicate children' -> case (boundInstance rt owner, getClassPredTys_maybe predicate) of
                (Just (cls, _), Just (selected, arguments)) -> cls == getName selected &&
                    not (null arguments) && eqType (last arguments) ty && all ordinary children'
                _ -> False
            _ -> lexicalMemorist ty evidence
        | otherwise = lexicalMemorist ty evidence
    lexicalMemorist ty evidence = case root evidence of
        Given predicate -> case getClassPredTys_maybe predicate of
            Just (cls, [_, carrier]) -> getName cls == runtimeName rt "Memorist.Base.Core" "Memorist" && eqType carrier ty
            Just (cls, [carrier]) -> getName cls == runtimeName rt "Memorist.Base.Core" "MonadMemorist" && eqType carrier ty
            _ -> False
        _ -> False
    safeRepr evidence = case root evidence of
        Given predicate -> reprPredicate predicate
        Instance owner predicate children' -> reprPredicate predicate && all safeRepr children' &&
            (home evidence || case boundInstance rt owner of
                Just (cls, _) -> cls == runtimeName rt "Memorist.Data.Interface.Data" "Repr"
                _ -> False)
        _ -> False
    reprPredicate predicate = case getClassPredTys_maybe predicate of
        Just (cls, [_]) -> getName cls == runtimeName rt "Memorist.Data.Interface.Data" "Repr"
        _ -> False
    representationCarrier = case (types, dictionaries) of
        ([_, _, carrier], [_, evidence]) -> memorist carrier evidence
        _ -> False
    demandPredicate evidence = case evidence of
        Super predicate _ -> demand predicate
        Given predicate -> demand predicate
        Instance _ predicate _ -> demand predicate
        CompilerTag{} -> False
        Unknown _ -> False
    demand predicate = case getClassPredTys_maybe predicate of
        Just (cls, [_]) -> getName cls == runtimeName rt "Memorist.Data.Interface.Demand" "Demand"
        _ -> False

uses :: Map.Map Name Family -> Environment -> Set.Set Name -> C.CoreExpr -> [Use]
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
            (C.Var identifier, arguments) | Just family <- selectedFamily identifier ->
                Use (getName identifier) family [ty | C.Type ty <- arguments]
                    [trace env given Set.empty argument | argument <- arguments,
                        not (C.isTyCoArg argument), isPredTy (exprType argument)] :
                    concat [walk env given argument | argument <- arguments,
                        not (C.isTyCoArg argument), not (isPredTy (exprType argument))]
            _ -> case expression of
                C.App function argument -> walk env given function ++ (if C.isTyCoArg argument then [] else walk env given argument)
                _ -> []
    selectedFamily identifier = case Map.lookup (getName identifier) selected of
        Just family -> Just family
        Nothing | isDefaultMethodOcc (nameOccName (getName identifier)) -> Just UnknownMethod
        Nothing | Just cls <- isClassOpId_maybe identifier, identifier `notElem` classSCSelIds cls -> Just UnknownMethod
        _ -> Nothing
    trace env given seen expression = case expression of
        C.Tick _ body -> trace env given seen body
        C.Cast body coercion | Just (level, ty) <- compilerTag body coercion -> CompilerTag level ty
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

-- GHC supplies this one newtype dictionary when stock Eq compares constructor
-- tags. No other dictionary cast, callback or source operation receives a role.
compilerTag :: C.CoreExpr -> Coercion -> Maybe (Type, Type)
compilerTag expression coercion = case (C.collectArgs expression, getClassPredTys_maybe (coercionRKind coercion)) of
    ((C.Var primitive, [C.Type actualLevel, C.Type actualTy]), Just (cls, [level, ty]))
        | getName cls == dataToTagClassName, isNewTyCon (classTyCon cls),
          isPrimOpId_maybe primitive `elem` map Just [DataToTagSmallOp, DataToTagLargeOp],
          eqType level actualLevel, eqType ty actualTy,
          eqCoercion coercion (mkSymCo (mkUnbranchedAxInstCo Representational (newTyConCo (classTyCon cls)) [level, ty] [])) -> Just (level, ty)
    _ -> Nothing

renderEvidence :: Evidence -> String
renderEvidence (Given ty) = "given " ++ pretty ty
renderEvidence (Instance name ty dependencies) = describeName name ++ " :: " ++ pretty ty ++
    concatMap ((" {" ++) . (++ "}") . renderEvidence) dependencies
renderEvidence (Super ty parent) = "super " ++ pretty ty ++ " of " ++ renderEvidence parent
renderEvidence (CompilerTag level ty) = "compiler constructor tag " ++ pretty level ++ " " ++ pretty ty
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
