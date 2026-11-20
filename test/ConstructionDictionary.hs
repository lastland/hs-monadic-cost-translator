-- | Finite selected dictionary syntax and substitution of field parameters.
module ConstructionDictionary (Evidence(..), Use(..), Environment, trace, renderEvidence,
    instantiateUse, instantiateTypes, substitute, predicateOf, closedFamilyStep, projected, descendant, sameEvidence, sameUse,
    Argument(..), proofScoped, hasApplication) where

import ConstructionAst (pretty)
import ConstructionHomeRepr (Field(..), HomeReprs, dictionaryBindings)
import ConstructionRuntime (Family, describeName)
import ConstructionTag (tagDictionary)
import ConstructionEvidenceTypes (Evidence(..), Argument(..), predicateOf)
import ConstructionSuper (validSuper)
import Control.Monad (guard)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC hiding (exprType, typeKind)
import GHC.Builtin.Types (eqDataCon, eqTyCon)
import qualified GHC.Core as C
import GHC.Core.Class (classAllSelIds, classSCSelIds, classTyCon, classTyVars)
import GHC.Core.Coercion (Role(..), coercionLKind, coercionRKind, coercionRole)
import GHC.Core.Coercion.Axiom (toBranchedAxiom, coAxiomRole, coAxiomTyCon)
import GHC.Core.TyCo.Rep (Coercion(..), MCoercion(..))
import GHC.Core.DataCon (dataConWorkId)
import GHC.Core.FVs (exprFreeVars)
import GHC.Core.FamInstEnv (emptyFamInstEnv, reduceTyFamApp_maybe)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.Opt.Arity (pushCoTyArg, pushCoValArg)
import GHC.Core.Reduction (reductionReducedType)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import qualified GHC.Core.TyCo.Subst as T
import GHC.Core.TyCon (isClosedSynFamilyTyConWithAxiom_maybe, newTyConCo_maybe, tyConSingleDataCon, isFamilyTyCon)
import GHC.Core.Type (getTyVar_maybe, isPredTy, splitTyConApp_maybe, substTyWith,
    splitForAllTyCoVar_maybe, splitFunTy_maybe, typeKind)
import GHC.Core.Utils (exprType)
import GHC.Types.Id (isDFunId, realIdUnfolding, setIdType)
import GHC.Types.Var (isId, isTyVar, isCoVar, varType)
import GHC.Types.Var.Set (elemVarSet)

data Use = Use Name Name Family [Type] [Evidence]
type Environment = Map.Map Name C.CoreExpr

trace :: Environment -> Set.Set Name -> Set.Set Name -> C.CoreExpr -> Evidence
trace _ _ _ expression | Just (p, t, op) <- tagDictionary expression = BuiltinTag p t op
trace env given seen expression = case expression of
    C.Tick _ body -> trace env given seen body
    C.Let binding body | all (\v -> isId v && isPredTy (idType v)) (C.bindersOf binding) ->
        trace (Map.union (Map.fromList [(getName v, rhs) | (v, rhs) <- C.flattenBinds [binding]]) env) given seen body
    C.Lam binder body | isTyVar binder, isPredTy (exprType expression) ->
        TypeLambda (exprType expression) binder (trace env given seen body)
    C.Lam binder body | isId binder, isPredTy (idType binder), isPredTy (exprType expression) ->
        EvidenceLambda (exprType expression) binder
            (trace env (Set.insert (getName binder) given) seen body)
    C.Case scrutinee binder target [C.Alt C.DEFAULT [] body]
        | isCoVar binder, not (binder `elemVarSet` exprFreeVars body),
          (C.Var selector, arguments) <- C.collectArgs scrutinee,
          Just cls <- tyConClass_maybe eqTyCon, selector `elem` classSCSelIds cls,
          [actual] <- [arg | arg <- arguments, not (C.isTyCoArg arg)],
          let source = trace env given seen actual, Just p <- predicateOf source,
          Just (equalityTyCon, [_, _, _]) <- splitTyConApp_maybe p, equalityTyCon == eqTyCon,
          validSuper selector (exprType scrutinee) source, eqType target (exprType body) ->
            EqualityCase target source (trace env given seen body)
    C.Cast body co | Just (_, sel) <- superclassAxiom Unpack co,
            let source = trace env given seen body, Just predicate <- predicateOf source, eqType predicate (coercionLKind co) ->
        Super sel (coercionRKind co) source
    C.Cast body co | Just source <- traceTransport co (trace env given seen body) -> source
    C.Cast body co | constraintTransport co,
            let source = trace env given seen body, Just predicate <- predicateOf source,
            eqType predicate (coercionLKind co) ->
        Transport (coercionRKind co) source
    C.Cast body co | liftedTransport co, let source = trace env given seen body,
        Just result <- certifiedCast (MCo co) source -> result
    C.Cast body co | equalityTransport co, let source = trace env given seen body,
        Just p <- predicateOf source, eqType p (coercionLKind co) ->
            Transport (coercionRKind co) source
    C.Cast{} -> Unknown "coerced dictionary"
    _ -> case C.collectArgs expression of
        (C.Var identifier, arguments)
            | identifier == dataConWorkId eqDataCon,
              [C.Type _, C.Type left, C.Type right, C.Coercion proof] <- arguments,
              nominalFamily proof, eqType left (coercionLKind proof), eqType right (coercionRKind proof) ->
                Equality (exprType expression) proof
            | isDFunId identifier -> Instance identifier (exprType expression) [ty | C.Type ty <- arguments]
                [trace env given seen arg | arg <- arguments, not (C.isTyCoArg arg), isPredTy (exprType arg)]
            | null arguments, getName identifier `Set.member` given -> Given (getName identifier) (idType identifier)
            | getName identifier `Set.notMember` seen, Just rhs <- Map.lookup (getName identifier) env,
              eqType (idType identifier) (exprType rhs) ->
                applyArguments (trace env given (Set.insert (getName identifier) seen) rhs) arguments
            | Just cls <- isClassOpId_maybe identifier, identifier `elem` classSCSelIds cls,
              let (prefix, rest) = splitAt (length (classTyVars cls) + 1) arguments,
              [arg] <- [a | a <- prefix, not (C.isTyCoArg a)] ->
                applyArguments (Super identifier (exprType (C.mkApps (C.Var identifier) prefix))
                    (trace env given seen arg)) rest
            | getName identifier `Set.member` given ->
                applyArguments (Given (getName identifier) (idType identifier)) arguments
        (function, arguments@(_:_)) -> applyArguments (trace env given seen function) arguments
        _ -> Unknown (pretty expression)
  where
    applyArguments = foldl (\function argument -> case argument of
        C.Type ty -> apply function (TypeArgument ty)
        _ | not (C.isTyCoArg argument), isPredTy (exprType argument) ->
            apply function (EvidenceArgument (trace env given seen argument))
        _ -> Unknown "non-evidence application")

renderEvidence :: Evidence -> String
renderEvidence (Given _ ty) = "given " ++ pretty ty
renderEvidence (Instance name ty _ dependencies) = describeName (getName name) ++ " :: " ++ pretty ty ++
    concatMap ((" {" ++) . (++ "}") . renderEvidence) dependencies
renderEvidence (Super _ ty parent) = "super " ++ pretty ty ++ " of " ++ renderEvidence parent
renderEvidence (Transport ty parent) = "transport " ++ pretty ty ++ " of " ++ renderEvidence parent
renderEvidence (Unknown why) = "unresolved " ++ why
renderEvidence (BuiltinTag p _ op) = "builtin " ++ describeName (getName op) ++ " :: " ++ pretty p
renderEvidence (TypeLambda p _ body) = "type-lambda " ++ pretty p ++ " {" ++ renderEvidence body ++ "}"
renderEvidence (EvidenceLambda p _ body) = "evidence-lambda " ++ pretty p ++ " {" ++ renderEvidence body ++ "}"
renderEvidence (Application p f arg) = "apply " ++ pretty p ++ " {" ++ renderEvidence f ++ "} {" ++ case arg of
    TypeArgument t -> pretty t ++ "}"
    EvidenceArgument d -> renderEvidence d ++ "}"
renderEvidence (Lifted co source) = "lifted " ++ pretty (coercionRKind co) ++ " {" ++ renderEvidence source ++ "}"
renderEvidence (Equality p _) = "boxed nominal equality " ++ pretty p
renderEvidence (EqualityCase _ equality body) = "equality-case {" ++ renderEvidence equality ++ "} {" ++ renderEvidence body ++ "}"

substitute :: T.Subst -> [(Name, Evidence)] -> Evidence -> Evidence
substitute substitution env ev = case ev of
    Given name t -> case lookup name env of
        Just actual | Just p <- predicateOf actual, eqType (ty t) p -> actual
                    | otherwise -> Unknown "field dictionary predicate mismatch"
        Nothing -> Given name (ty t)
    Instance df p ts ds -> Instance df (ty p) (map ty ts) (map sub ds)
    Super sel p d -> Super sel (ty p) (sub d)
    Transport p d -> Transport (ty p) (sub d)
    Unknown{} -> ev
    BuiltinTag p t op -> BuiltinTag (ty p) (ty t) op
    TypeLambda p v body ->
        let (inner, v') = T.substTyVarBndr substitution v
        in if any (\(_, actual) -> mentionsType v actual || mentionsType v' actual) env
           then Unknown "capturing quantified type substitution"
           else TypeLambda (ty p) v' (substitute inner env body)
    EvidenceLambda p v body
        | any (Set.member (getName v) . freeNames . snd) outer -> Unknown "capturing quantified evidence substitution"
        | otherwise -> EvidenceLambda (ty p) (setIdType v (ty (idType v)))
            (substitute substitution outer body)
      where outer = filter ((/= getName v) . fst) env
    Application p f arg -> Application (ty p) (sub f) (case arg of
        TypeArgument t -> TypeArgument (ty t)
        EvidenceArgument d -> EvidenceArgument (sub d))
    Lifted co source -> Lifted (T.substCo substitution co) (sub source)
    Equality p proof -> Equality (ty p) (T.substCo substitution proof)
    EqualityCase p equality body -> EqualityCase (ty p) (sub equality) (sub body)
  where
    ty = T.substTy substitution
    sub = substitute substitution env

freeNames :: Evidence -> Set.Set Name
freeNames (Given n _) = Set.singleton n
freeNames (EvidenceLambda _ v body) = Set.delete (getName v) (freeNames body)
freeNames evidence = Set.unions (map freeNames (proofChildren evidence))

-- Evidence-name capture fails closed; GHC handles type and kind binders.
mentionsType :: TyVar -> Evidence -> Bool
mentionsType v evidence =
    any (\t -> v `elemVarSet` tyCoVarsOfType t) extra || any (mentionsType v) (proofChildren evidence)
  where
    extra = maybe [] pure (predicateOf evidence) ++ case evidence of
        Instance _ _ ts _ -> ts
        Application _ _ (TypeArgument t) -> [t]
        _ -> []

proofChildren :: Evidence -> [Evidence]
proofChildren (Instance _ _ _ ds) = ds
proofChildren (Super _ _ parent) = [parent]
proofChildren (Transport _ parent) = [parent]
proofChildren (TypeLambda _ _ body) = [body]
proofChildren (EvidenceLambda _ _ body) = [body]
proofChildren (Lifted _ parent) = [parent]
proofChildren (Application _ f (TypeArgument _)) = [f]
proofChildren (Application _ f (EvidenceArgument d)) = [f, d]
proofChildren (EqualityCase _ p body) = [p, body]
proofChildren _ = []

resultType :: Type -> Argument -> Maybe Type
resultType function (TypeArgument actual) = do
    (v, body) <- splitForAllTyCoVar_maybe function
    guard (isTyVar v && eqType (varType v) (typeKind actual))
    pure (substTyWith [v] [actual] body)
resultType function (EvidenceArgument actual) = do
    (_, _, formal, result) <- splitFunTy_maybe function
    predicate <- predicateOf actual
    guard (isPredTy formal && eqType formal predicate)
    pure result

validApplication :: Type -> Evidence -> Argument -> Bool
validApplication target function argument = maybe False (eqType target)
    (predicateOf function >>= (`resultType` argument))

apply :: Evidence -> Argument -> Evidence
apply function argument = case predicateOf function >>= (`resultType` argument) of
    Nothing -> Unknown "quantified evidence argument/type mismatch"
    Just target -> case (function, argument) of
        (TypeLambda _ v body, TypeArgument actual) ->
            checked target (substitute (T.zipTvSubst [v] [actual]) [] body)
        (EvidenceLambda _ v body, EvidenceArgument actual) ->
            checked target (substitute T.emptySubst [(getName v, actual)] body)
        (Instance df _ ts ds, TypeArgument actual) -> Instance df target (ts ++ [actual]) ds
        (Instance df _ ts ds, EvidenceArgument actual) -> Instance df target ts (ds ++ [actual])
        (Lifted co source, TypeArgument actual) -> case do
            (actual', residual) <- pushCoTyArg co actual
            certifiedCast residual (apply source (TypeArgument actual')) of
                Just result -> checked target result
                Nothing -> Unknown "quantified type-cast application"
        (Lifted co source, EvidenceArgument actual) -> case do
            (contravariant, residual) <- pushCoValArg co
            actual' <- certifiedCast contravariant actual
            certifiedCast residual (apply source (EvidenceArgument actual')) of
                Just result -> checked target result
                Nothing -> Unknown "quantified evidence-cast application"
        _ -> Application target function argument
  where
    checked p result | maybe False (eqType p) (predicateOf result) = result
                     | otherwise = Unknown "quantified evidence result mismatch"

-- Provenance only: local proof binders do not grant method permission.
proofScoped :: (Name -> Bool) -> Evidence -> Bool
proofScoped owned = go owned
  where
    go scope (Given n _) = scope n
    go scope (Instance _ _ _ ds) = all (go scope) ds
    go scope (Transport _ d) = go scope d
    go scope (Lifted co d) = maybe False (eqType (coercionLKind co)) (predicateOf d) && go scope d
    go scope (Super selector target parent) = validSuper selector target parent && go scope parent
    go scope (TypeLambda _ _ body) = go scope body
    go scope (EvidenceLambda _ v body) = go (\n -> n == getName v || scope n) body
    go scope (Application target f a) = validApplication target f a && go scope f &&
        case a of TypeArgument _ -> True; EvidenceArgument d -> go scope d
    go _ Equality{} = True
    go scope (EqualityCase p equality body) = maybe False (eqType p) (predicateOf body) &&
        go scope equality && go scope body
    go _ _ = False

hasApplication :: Evidence -> Bool
hasApplication Application{} = True
hasApplication (Super _ _ parent) = hasApplication parent
hasApplication (Transport _ parent) = hasApplication parent
hasApplication (Lifted _ parent) = hasApplication parent
hasApplication _ = False

-- Only structural lifts of validated nominal closed-family equations.
liftedTransport :: Coercion -> Bool
liftedTransport = lifted
  where
    lifted (SubCo co) = nominal co
    lifted (SymCo co) = lifted co
    lifted (ForAllCo v left right kind body) =
        isTyVar v && left == right && reflexive kind && lifted body
    lifted (FunCo Representational left right multiplicity argument result) =
        left == right && reflexive multiplicity && lifted argument && lifted result
    lifted (GRefl Representational _ MRefl) = True
    lifted co = constraintTransport co
    nominal (ForAllCo v left right kind body) =
        isTyVar v && left == right && reflexive kind && nominal body
    nominal (FunCo Nominal left right multiplicity argument result) =
        left == right && reflexive multiplicity && nominal argument && nominal result
    nominal (SymCo co) = nominal co
    nominal co = reflexive co || constraintTransport (SubCo co)
    reflexive Refl{} = True
    reflexive (GRefl Nominal _ MRefl) = True
    reflexive _ = False

-- Inert boxed equality fields retain nominal family proofs, never coercion
-- variables or arbitrary casts. They confer no runtime method permission.
equalityTransport :: Coercion -> Bool
equalityTransport co = equality (coercionLKind co) && equality (coercionRKind co) && lifted co
  where
    equality t = case splitTyConApp_maybe t of Just (tc, _) -> tc == eqTyCon; _ -> False
    lifted (SubCo proof) = nominalFamily proof
    lifted (SymCo proof) = lifted proof
    lifted (TyConAppCo Representational tc proofs) = tc == eqTyCon && all nominalFamily proofs
    lifted _ = False

nominalFamily :: Coercion -> Bool
nominalFamily co | coercionRole co /= Nominal = False
nominalFamily Refl{} = True
nominalFamily (GRefl Nominal _ MRefl) = True
nominalFamily (SymCo co) = nominalFamily co
nominalFamily (TransCo a b) = eqType (coercionRKind a) (coercionLKind b) &&
    nominalFamily a && nominalFamily b
nominalFamily (TyConAppCo Nominal _ arguments) = all nominalFamily arguments
nominalFamily (AppCo a b) = nominalFamily a && nominalFamily b
nominalFamily (AxiomInstCo ax _ arguments) =
    coAxiomRole ax == Nominal && isFamilyTyCon (coAxiomTyCon ax) && all nominalFamily arguments
nominalFamily _ = False

-- Only called after source validation or on GHC push-rule derivations.
certifiedCast :: MCoercion -> Evidence -> Maybe Evidence
certifiedCast MRefl source = Just source
certifiedCast (MCo co) source = do
    predicate <- predicateOf source
    guard (eqType predicate (coercionLKind co))
    let target = coercionRKind co
    pure $ if eqType predicate target then source else
        case (splitForAllTyCoVar_maybe target, splitFunTy_maybe target) of
            (Nothing, Nothing) -> Transport target source
            _ -> Lifted co source

instantiateUse :: Field -> [Type] -> [Evidence] -> Evidence -> Maybe (Use -> Use)
instantiateUse field args ds carrier = do
    (formalClass, formal) <- getClassPredTys_maybe (idType (carrierBinder field))
    (cls, actual) <- predicateOf carrier >>= getClassPredTys_maybe
    vars <- mapM getTyVar_maybe formal
    guard (length vars == length actual && length (parameters field) == length args && length (slots field) == length ds)
    let substitution = T.zipTvSubst (parameters field ++ vars) (args ++ actual)
        ty = T.substTy substitution
        env = (getName (carrierBinder field), carrier) : [(name, d) | (names, d) <- zip (slots field) ds, name <- names]
    guard (cls == formalClass)
    pure (\(Use n c f ts es) -> Use n c f (map ty ts) (map (substitute substitution env) es))

instantiateTypes :: [TyVar] -> [Type] -> [Use] -> Maybe [Use]
instantiateTypes variables arguments obligations = do
    guard (length variables == length arguments)
    let substitution = T.zipTvSubst variables arguments
        ty = T.substTy substitution
    pure [Use n c f (map ty ts) (map (substitute substitution []) es) | Use n c f ts es <- obligations]

data Direction = Pack | Unpack deriving (Eq)

-- A single nominal step is sufficient here and does not chase recursive
-- equations. GHC selects the branch; closed families need no instance env.
closedFamilyStep :: Type -> Maybe Type
closedFamilyStep ty = do
    (tc, args) <- splitTyConApp_maybe ty
    _ <- isClosedSynFamilyTyConWithAxiom_maybe tc
    reductionReducedType <$> reduceTyFamApp_maybe (emptyFamInstEnv, emptyFamInstEnv) Nominal tc args

-- This transport peels a class lifted over the reverse equation.
-- Keep the selected DFun as evidence rather than looking up a replacement.
familyTransport :: Coercion -> Bool
familyTransport co@(TyConAppCo Representational tc [SymCo step@(AxiomInstCo ax _ _)]) =
    case (getClassPredTys_maybe (coercionLKind co), splitTyConApp_maybe (coercionLKind step)) of
        (Just (cls, [_]), Just (family, _)) -> classTyCon cls == tc &&
            isClosedSynFamilyTyConWithAxiom_maybe family == Just ax &&
            maybe False (eqType (coercionRKind step)) (closedFamilyStep (coercionLKind step))
        _ -> False
familyTransport (SubCo (SymCo (TyConAppCo Nominal tc [step]))) =
    familyTransport (TyConAppCo Representational tc [SymCo step])
familyTransport (SubCo (TyConAppCo Nominal tc [SymCo step])) =
    familyTransport (TyConAppCo Representational tc [SymCo step])
familyTransport _ = False

-- Retain both predicates for exact context-slot substitution. Only the actual
-- nominal closed-family equation with reflexive arguments may transport a
-- constraint; endpoint equality alone does not establish its provenance.
constraintTransport :: Coercion -> Bool
constraintTransport (SubCo step) = nominal step
  where
    nominal (SymCo inner) = equationStep inner
    nominal inner = equationStep inner
    equationStep equation@(AxiomInstCo ax _ xs) = case splitTyConApp_maybe (coercionLKind equation) of
        Just (tc, _) -> isPredTy (coercionLKind equation) && isPredTy (coercionRKind equation) &&
            isClosedSynFamilyTyConWithAxiom_maybe tc == Just ax && all reflexive xs &&
            maybe False (eqType (coercionRKind equation)) (closedFamilyStep (coercionLKind equation))
        _ -> False
    equationStep _ = False
    reflexive Refl{} = True
    reflexive (GRefl Nominal _ MRefl) = True
    reflexive _ = False
constraintTransport (SymCo (SubCo step)) = constraintTransport (SubCo step)
constraintTransport _ = False

traceTransport :: Coercion -> Evidence -> Maybe Evidence
traceTransport co source = do
    guard (familyTransport co)
    Instance _ predicate _ _ <- pure source
    guard (eqType predicate (coercionLKind co))
    pure source

-- A class with a single superclass and no methods may have a newtype
-- dictionary. Its own newtype axiom (possibly eta-reduced and applied) is
-- the constructor in reverse and the selector forward. Endpoint equality
-- alone is not a certificate for casts, compositions or arbitrary coercions.
superclassAxiom :: Direction -> Coercion -> Maybe (Class, Id)
superclassAxiom direction co = do
    let (parent, child) = case direction of
            Pack -> (coercionRKind co, coercionLKind co)
            Unpack -> (coercionLKind co, coercionRKind co)
    (cls, arguments) <- getClassPredTys_maybe parent
    [sel] <- pure (classAllSelIds cls)
    [super] <- pure (classSCTheta cls)
    guard (classSCSelIds cls == [sel] && eqType child (substTyWith (classTyVars cls) arguments super))
    axiom <- newTyConCo_maybe (classTyCon cls)
    let match want (SymCo inner) args | want == Pack = match Unpack inner args
        match want (AppCo f x) args = do
            t <- reflexive x
            match want f (t : args)
        match Unpack (AxiomInstCo actual 0 xs) args = do
            ts <- mapM reflexive xs
            guard (actual == toBranchedAxiom axiom && sameList eqType arguments (ts ++ args))
        match _ _ _ = Nothing
        reflexive (Refl t) = Just t
        reflexive (GRefl _ t MRefl) = Just t
        reflexive _ = Nothing
    match direction co []
    pure (cls, sel)

-- Follow the selected superclass field of the actual DFun. Never certify a
-- superclass method merely because the parent instance was registered.
projected :: HomeReprs -> Evidence -> Maybe Evidence
projected homes ev | case ev of Super{} -> True; Transport{} -> True; Application{} -> True; Lifted{} -> True; _ -> False = normalize [] ev
  where
    normalize path (Super sel predicate parent) = do
        guard (validSuper sel predicate parent)
        origin@(Instance df p ts ds) <- normalize path parent
        (cls, _) <- getClassPredTys_maybe p
        guard (sel `elem` classSCSelIds cls && all (progress sel df origin) path)
        let constructor = tyConSingleDataCon (classTyCon cls)
            fromBody rhs = do
                let (bs, body) = C.collectBinders rhs
                    typeBinders = filter isTyVar bs
                guard (length typeBinders == length ts && eqType (substTyWith typeBinders ts (exprType body)) p)
                -- GHC may fuse field transport and newtype packing into
                -- one cast, or bind the transported dictionary separately.
                case body of
                    C.Cast payload (TransCo transport pack) |
                        Just (owner, selected) <- superclassAxiom Pack pack, owner == cls && selected == sel,
                        eqType (coercionRKind transport) (coercionLKind pack) ->
                            pure (bs, [payload], Just transport)
                    C.Cast payload co | Just (owner, selected) <- superclassAxiom Pack co,
                        owner == cls && selected == sel && eqType (exprType payload) (coercionLKind co) ->
                            pure (bs, [payload], Nothing)
                    _ -> do
                        (C.Var worker, args) <- pure (C.collectArgs body)
                        guard (worker == dataConWorkId constructor)
                        pure (bs, args, Nothing)
        (binders, args, transport) <- case Map.lookup (getName df) (dictionaryBindings homes) of
            Just rhs -> fromBody rhs
            Nothing -> case realIdUnfolding df of
                C.DFunUnfolding bs con args | con == constructor -> pure (bs, args, Nothing)
                C.CoreUnfolding{C.uf_tmpl = rhs} | isNewTyCon (classTyCon cls) -> fromBody rhs
                _ -> Nothing
        let variables = filter isTyVar binders
            formals = filter isId binders
            fields = filter (not . C.isTyCoArg) args
        guard (length variables == length ts && length formals == length ds && length fields == length (classAllSelIds cls))
        guard (all (isPredTy . idType) formals &&
            and (zipWith (\v d -> maybe False (eqType (substTyWith variables ts (idType v))) (predicateOf d)) formals ds))
        field <- lookup sel (zip (classAllSelIds cls) fields)
        let names = map getName formals
            source = trace (dictionaryBindings homes) (Set.fromList names) Set.empty field
        template <- maybe (pure source) (`traceTransport` source) transport
        result <- normalize ((sel, df, origin) : path) (substitute (T.zipTvSubst variables ts) (zip names ds) template)
        actual <- predicateOf result
        guard (eqType predicate actual || case (getClassPredTys_maybe predicate, getClassPredTys_maybe actual) of
            (Just (wanted, [operand]), Just (found, [ty])) ->
                wanted == found && maybe False (eqType ty) (closedFamilyStep operand)
            _ -> False)
        pure result
    normalize path (Instance df ty ts ds) = Instance df ty ts <$> mapM (normalize path) ds
    normalize path (Application target function argument) = do
        guard (validApplication target function argument)
        f <- normalize path function
        a <- case argument of
            TypeArgument t -> pure (TypeArgument t)
            EvidenceArgument d -> EvidenceArgument <$> normalize path d
        let result = apply f a
        guard (maybe False (eqType target) (predicateOf result))
        if sameEvidence result (Application target f a)
            then pure result
            else normalize path result
    normalize _ term@TypeLambda{} = pure term
    normalize _ term@EvidenceLambda{} = pure term
    normalize path (Lifted co source) = normalize path source >>= certifiedCast (MCo co)
    normalize path (Transport ty source) = do
        result <- normalize path source
        -- Both wrappers were validated when traced. After exact substitution,
        -- a forward/inverse pair recovers the original selected dictionary.
        pure $ case result of
            Transport _ parent | maybe False (eqType ty) (predicateOf parent) -> parent
            _ | maybe False (eqType ty) (predicateOf result) -> result
              | otherwise -> Transport ty result
    normalize _ given@Given{} = Just given
    normalize _ Unknown{} = Nothing
    normalize _ builtin@BuiltinTag{} = Just builtin
    normalize _ proof@Equality{} = Just proof
    normalize path (EqualityCase p equality body) = do
        proof <- normalize path equality
        guard (proofScoped (const False) proof)
        result <- normalize path body
        guard (maybe False (eqType p) (predicateOf result))
        pure result
    progress sel df origin (prevSel, prevDf, prev) = sel /= prevSel || df /= prevDf || descendant origin prev
projected _ ev = Just ev

-- Allow another application of the same field only on an actual, strictly
-- nested dictionary from the previous call. Synthetic growing cycles stop.
descendant :: Evidence -> Evidence -> Bool
descendant target (Instance _ _ _ ds) = any (\d -> sameEvidence target d || descendant target d) ds
descendant target (Super _ _ parent) = sameEvidence target parent || descendant target parent
descendant target (Transport _ parent) = sameEvidence target parent || descendant target parent
descendant _ _ = False

sameEvidence :: Evidence -> Evidence -> Bool
sameEvidence (Instance a p as ds) (Instance b q bs es) = a == b && eqType p q && sameList eqType as bs && sameList sameEvidence ds es
sameEvidence (Given a p) (Given b q) = a == b && eqType p q
sameEvidence (Super a p d) (Super b q e) = a == b && eqType p q && sameEvidence d e
sameEvidence (Transport p d) (Transport q e) = eqType p q && sameEvidence d e
sameEvidence (Unknown x) (Unknown y) = x == y
sameEvidence (BuiltinTag p t op) (BuiltinTag q u other) = eqType p q && eqType t u && op == other
sameEvidence (TypeLambda p v d) (TypeLambda q w e) = eqType p q && v == w && sameEvidence d e
sameEvidence (EvidenceLambda p v d) (EvidenceLambda q w e) = eqType p q && v == w && sameEvidence d e
sameEvidence (Application p f a) (Application q g b) = eqType p q && sameEvidence f g && sameArgument a b
sameEvidence (Lifted co d) (Lifted other e) =
    eqType (coercionLKind co) (coercionLKind other) &&
    eqType (coercionRKind co) (coercionRKind other) && sameEvidence d e
sameEvidence (Equality p a) (Equality q b) = eqType p q &&
    eqType (coercionLKind a) (coercionLKind b) && eqType (coercionRKind a) (coercionRKind b)
sameEvidence (EqualityCase p a d) (EqualityCase q b e) = eqType p q &&
    sameEvidence a b && sameEvidence d e
sameEvidence _ _ = False

sameArgument :: Argument -> Argument -> Bool
sameArgument (TypeArgument a) (TypeArgument b) = eqType a b
sameArgument (EvidenceArgument a) (EvidenceArgument b) = sameEvidence a b
sameArgument _ _ = False

sameList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
sameList f xs ys = length xs == length ys && and (zipWith f xs ys)

sameUse :: Use -> Use -> Bool
sameUse (Use a c f ts ds) (Use b d g us es) = a == b && c == d && f == g && sameList eqType ts us && sameList sameEvidence ds es
