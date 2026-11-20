module Cost.Translate.Frontend.Polymorphic
    ( constructorPayload
    , phantomAllocation
    , allocatePayload
    , Payload
    , producerPayload
    , emitPayload
    , payloadType
    ) where

import Control.Applicative ((<|>))
import Control.Monad (guard, zipWithM)
import Cost.Translate.Callable (spine)
import Cost.Translate.Datatypes (renderType)
import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.Expressions
    ( conditionalAction
    , reuseThunk
    , suspendArgument
    )
import Cost.Translate.Frontend.LocalValues (bodyTypeRenamings, usedNames)
import Cost.Translate.Frontend.NamePlan (NamePlan (..))
import Cost.Translate.Frontend.Resolve (resolvedConstructor)
import Cost.Translate.Frontend.Signatures
    ( monadicType
    , thunkType
    , translatedSignatureAs
    )
import Cost.Translate.Frontend.Syntax (argumentKey, collect, plainRhs)
import Cost.Translate.Kinds (renderBinder)
import Cost.Translate.Output qualified as O
import Cost.Translate.Sharing (nativeSharing, renameAllocation, sharingType)
import Cost.Translate.Sharing.Schema (planNativeSchemas)
import Cost.Translate.Sharing.Structural (phantomBranches)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC
import GHC.Core.DataCon (dataConInstOrigArgTys)
import GHC.Core.TyCo.FVs (tyCoVarsOfType, tyCoVarsOfTypesWellScoped)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.TyCon (isAlgTyCon)
import GHC.Core.Type
    ( getTyVar_maybe
    , isLiftedTypeKind
    , isManyTy
    , mkSpecForAllTys
    , mkTyVarTy
    , splitForAllForAllTyBinders
    , splitForAllTyVars
    , splitFunTy_maybe
    , splitTyConApp_maybe
    , substTyWith
    , tyVarKind
    )
import GHC.Tc.Utils.TcType (tcSplitSigmaTy)
import GHC.Types.Name (getOccString)
import GHC.Types.Unique.Set (elementOfUniqSet)
import GHC.Types.Var
    ( FunTyFlag (FTF_T_T)
    , binderFlag
    , binderVar
    , isInferredForAllTyFlag
    , setTyVarKind
    )

data PayloadRoute = OriginalPayload | CheckedPayload
data Payload = Payload PayloadRoute [TyVar] Type PayloadBody
data PayloadBody
    = ConstructorPayload String [(String, Field)]
    | ConditionalPayload (LHsExpr GhcRn) Payload Payload
    | ApplicationPayload String String Payload
    | ProducerCall String String (LHsExpr GhcRn)
data Field = Ordinary (LHsExpr GhcRn) | Generalized Payload

constructorPayload ::
    Env ->
    Name ->
    Type ->
    Match GhcRn (LHsExpr GhcRn) ->
    Maybe ([TyVar], Payload)
constructorPayload env name ty match = do
    expression <- plainRhs match
    let renamings = bodyTypeRenamings Map.empty env name (O.tuple []) ty
        (variables, body) = sharingType renamings ty
        plan route scoped =
            (scoped,) <$> case Map.lookup name (producerCalls env) of
                Just worker -> do
                    (_, [argument]) <- pure (spine expression)
                    let key = argumentKey expression
                    [slot] <- pure (argumentNames env key 1)
                    let call = ProducerCall worker slot argument
                    pure (Payload route variables body call)
                Nothing -> planPayload route env variables body expression
        original = do
            scoped <- originalScope env name ty variables
            plan OriginalPayload scoped
        checked = do
            scoped <- checkedScope env name ty variables
            plan CheckedPayload scoped
    guard (not (null variables))
    original <|> checked

checkedScope :: Env -> Name -> Type -> [TyVar] -> Maybe [TyVar]
checkedScope env name ty variables = do
    let schemas = plannedNativeSchemas (namePlan env)
    Right Nothing <- pure (nativeSharing schemas Map.empty ty)
    scoped <- payloadScope env name ty
    Right _ <- pure (payloadBinders CheckedPayload env variables)
    pure scoped

phantomAllocation ::
    Map.Map Name O.Ty ->
    Env ->
    Name ->
    O.Expr ->
    Type ->
    Maybe (O.Expr -> O.Expr)
phantomAllocation signatures env name action ty = do
    let (variables, result) = splitForAllTyVars ty
        datatypes = sourceDatatypes env
    guard (not (null variables))
    branches <- phantomBranches datatypes variables result
    _ <- checkedScope env name ty variables
    Right (allocatorType, payload) <- pure (phantomTypes env name action ty)
    producer <- case Map.lookup name signatures of
        Nothing -> Just action
        Just _ -> do
            Right source <-
                pure (translatedSignatureAs False monadicType datatypes 0 ty)
            pure (O.ann action source)
    let plan = namePlan env
        local = localName env name
    root <- Map.lookup name (plannedAnchors plan)
    allocate <- Map.lookup name (plannedBodies plan)
    let value = O.caseE (O.v "produced") branches
        retag = O.app (typedOperation "pure" payload) [value]
        body =
            suspendPayload payload $
                O.doE [O.bind "produced" (O.v "producer")] retag
        definition = O.val allocate [O.p "producer"] body
        view = O.call "mapThunk" [instantiatePayload, O.v root]
        shared =
            [O.sig local sig | Just sig <- [Map.lookup name signatures]]
                ++ [O.val local [] view]
        allocation = O.bind root (O.call allocate [producer])
        bind rest
            | local `Set.notMember` usedNames rest = rest
            | otherwise =
                O.letE
                    [O.sig allocate allocatorType, definition]
                    (O.doE [allocation] (O.letE shared rest))
    pure bind

phantomTypes :: Env -> Name -> O.Expr -> Type -> Either String (O.Ty, O.Ty)
phantomTypes env name action ty = do
    let captures = tyCoVarsOfTypesWellScoped [ty]
        closed = mkSpecForAllTys captures ty
        names = bodyTypeRenamings Map.empty env name action closed
        (variables, input) = splitForAllTyVars (renameAllocation names closed)
        (fixed, varying) = splitAt (length captures) variables
        binders = payloadBinders CheckedPayload env
    outer <- binders fixed
    inner <- binders varying
    rendered <- renderType (sourceDatatypes env) input
    let payload = O.forallT inner rendered
        rhsType = O.forallT inner (monadicType rendered)
        rootType = monadicType (thunkType payload)
    pure (O.forallT outer (O.arrow rhsType rootType), payload)

originalScope :: Env -> Name -> Type -> [TyVar] -> Maybe [TyVar]
originalScope env name ty variables = do
    guard (all (payloadKind . tyVarKind) variables)
    guard (null names || map getName original == names)
    pure (if null names then [] else original)
    where
        names = Map.findWithDefault [] name (scopedTypeVariables env)
        original = fst (splitForAllTyVars ty)

payloadKind :: Type -> Bool
payloadKind kind
    | isLiftedTypeKind kind = True
    | Just (FTF_T_T, multiplicity, input, result) <- splitFunTy_maybe kind =
        isManyTy multiplicity
            && isLiftedTypeKind input
            && isLiftedTypeKind result
    | otherwise = False

payloadScope :: Env -> Name -> Type -> Maybe [TyVar]
payloadScope env name ty
    | null names = Just []
    | otherwise = do
        guard (map (getName . binderVar) explicit == names)
        _ <-
            either
                (const Nothing)
                Just
                (payloadBinders CheckedPayload env original)
        pure original
    where
        names = Map.findWithDefault [] name (scopedTypeVariables env)
        original = fst (splitForAllTyVars ty)
        explicit =
            filter
                ( \binder ->
                    not (isInferredForAllTyFlag (binderFlag binder))
                        || getName (binderVar binder) `elem` names
                )
                (fst (splitForAllForAllTyBinders ty))

planPayload ::
    PayloadRoute -> Env -> [TyVar] -> Type -> LHsExpr GhcRn -> Maybe Payload
planPayload route env variables ty expression = do
    let (headExpression, arguments) = spine expression
    body <- case unLoc headExpression of
        HsIf _ condition yes no
            | null arguments ->
                ConditionalPayload condition
                    <$> planPayload route env variables ty yes
                    <*> planPayload route env variables ty no
        HsVar _ located
            | [argument] <- arguments
            , Just target <- payloadFunction env (unLoc located) -> do
                [slot] <- pure (argumentNames env (argumentKey expression) 1)
                ApplicationPayload target slot
                    <$> planPayload route env variables ty argument
        HsVar _ located -> do
            let name = unLoc located
            (tc, types) <- splitTyConApp_maybe ty
            guard (isAlgTyCon tc)
            constructor <-
                lookup name [(getName dc, dc) | dc <- tyConDataCons tc]
            guard (isVanillaDataCon constructor)
            target <- resolvedConstructor env name
            let fields =
                    map scaledThing (dataConInstOrigArgTys constructor types)
                site = argumentKey expression
                names = argumentNames env site (length arguments)
            guard (length fields == length arguments)
            planned <- zipWithM field fields arguments
            pure (ConstructorPayload target (zip names planned))
        _ -> Nothing
    pure (Payload route variables ty body)
    where
        field ty' argument =
            case filter dependent variables of
                [] -> Just (Ordinary argument)
                quantified -> do
                    nested <- planPayload route env quantified ty' argument
                    pure (Generalized nested)
            where
                dependent variable =
                    variable `elementOfUniqSet` tyCoVarsOfType ty'

producerPayload ::
    Env -> Name -> TyVar -> Type -> LHsExpr GhcRn -> Maybe Payload
producerPayload env formal variable result expression = do
    guard (null (collect expression :: [LHsType GhcRn]))
    payload@(Payload _ _ _ (ConstructorPayload _ fields)) <-
        planPayload CheckedPayload env [variable] result expression
    guard (not (null [() | (_, Generalized _) <- fields]) && constructs payload)
    pure payload
    where
        constructs (Payload _ _ _ (ConstructorPayload _ fields)) =
            all (supported . snd) fields
        constructs _ = False
        supported (Generalized nested) = constructs nested
        supported (Ordinary argument) = case spine argument of
            (L _ (HsVar _ n), []) -> unLoc n == formal
            _ -> False

payloadFunction :: Env -> Name -> Maybe String
payloadFunction env name = do
    guard (isExternalName name)
    Function target 1 <- lookupEnv name env
    ty <- Map.lookup name (inferredBindings env)
    case tcSplitSigmaTy ty of
        ([variable], [], body)
            | isLiftedTypeKind (tyVarKind variable)
            , Just (FTF_T_T, multiplicity, argument, result) <-
                splitFunTy_maybe body
            , isManyTy multiplicity
            , getTyVar_maybe argument == Just variable
            , getTyVar_maybe result == Just variable ->
                Just target
        _ -> Nothing

payloadType :: Env -> Payload -> Either String O.Ty
payloadType env (Payload route variables ty _) =
    O.forallT
        <$> payloadBinders route env variables
        <*> renderType (sourceDatatypes env) ty

payloadBinders :: PayloadRoute -> Env -> [TyVar] -> Either String [O.Binder]
payloadBinders route env variables = case route of
    OriginalPayload -> mapM binder variables
    CheckedPayload -> do
        guardNames
        mapM (binder . normalized) variables
    where
        captured =
            filter
                (`notElem` variables)
                (tyCoVarsOfTypesWellScoped (map tyVarKind variables))
        occurrences = map getOccString (variables ++ captured)
        guardNames
            | length occurrences == Set.size (Set.fromList occurrences) =
                Right ()
            | otherwise = Left "ambiguous payload binder scope"
        kinds = substTyWith variables (map mkTyVarTy variables)
        normalized variable =
            setTyVarKind variable (kinds (tyVarKind variable))
        binder variable
            | isLiftedTypeKind (tyVarKind variable) =
                pure (O.b (getOccString variable))
            | otherwise = renderBinder (sourceDatatypes env) variable

typedOperation :: String -> O.Ty -> O.Expr
typedOperation name = O.typeApp (O.typeApp (O.v name) (O.t "M.MemoristM"))

suspendPayload :: O.Ty -> O.Expr -> O.Expr
suspendPayload ty action = O.app (typedOperation "M.lazy" ty) [action]

payloadView :: O.Ty -> O.Expr -> O.Expr
payloadView ty thunk =
    O.app
        (O.typeApp (O.v "mapThunk") ty)
        [instantiatePayload, thunk]

instantiatePayload :: O.Expr
instantiatePayload = O.lam [O.p "payload"] (O.v "payload")

emitPayload ::
    (LHsExpr GhcRn -> Either String O.Expr) ->
    Env ->
    Payload ->
    Either String O.Expr
emitPayload emit env payload@(Payload _ _ _ body) = case body of
    ProducerCall worker name expression -> do
        (statements, thunk) <- field (name, Ordinary expression)
        pure (O.doE statements (O.call worker [thunk]))
    ApplicationPayload target name argument -> do
        ty <- payloadType env argument
        action <- emitPayload emit env argument
        pure
            ( O.doE
                [O.bind name (suspendPayload ty action)]
                (O.app (O.typeApp (O.v target) ty) [O.v name])
            )
    ConditionalPayload condition yes no ->
        conditionalAction
            <$> emit condition
            <*> emitPayload emit env yes
            <*> emitPayload emit env no
    ConstructorPayload con fields -> do
        ty <- payloadType env payload
        arguments <- mapM field fields
        pure
            ( O.doE
                (concatMap fst arguments)
                ( O.app
                    (typedOperation "pure" ty)
                    [O.call con (map snd arguments)]
                )
            )
    where
        field (name, Ordinary expression) = case reuseThunk env expression of
            Just thunk -> pure ([], thunk)
            Nothing -> suspendArgument name <$> emit expression
        field (name, Generalized nested) = do
            ty <- payloadType env nested
            action <- emitPayload emit env nested
            pure
                ( [O.bind name (suspendPayload ty action)]
                , payloadView ty (O.v name)
                )

allocatePayload ::
    (LHsExpr GhcRn -> Either String O.Expr) ->
    Env ->
    Name ->
    ([TyVar], Payload) ->
    O.Expr ->
    Either String O.Expr
allocatePayload emit env name allocation@(_, payload) continuation
    | let used = usedNames continuation
    , localName env name `Set.notMember` used = case payload of
        Payload _ _ _ ProducerCall {} -> validate
        Payload OriginalPayload _ _ _
            | Just ty <- Map.lookup name (inferredBindings env)
            , Right Nothing <-
                nativeSharing (planNativeSchemas Set.empty []) Map.empty ty ->
                pure continuation
        _ -> validate
    | otherwise = allocateUsedPayload emit env name allocation continuation
    where
        validate = emitPayload emit env payload >> pure continuation

allocateUsedPayload ::
    (LHsExpr GhcRn -> Either String O.Expr) ->
    Env ->
    Name ->
    ([TyVar], Payload) ->
    O.Expr ->
    Either String O.Expr
allocateUsedPayload emit env name (scoped, payload) continuation = do
    ty <- payloadType env payload
    action <- emitPayload emit env payload
    sourceBinders <- mapM (renderBinder (sourceDatatypes env)) scoped
    let local = localName env name
        root =
            Map.findWithDefault
                (local ++ "anchor")
                name
                (plannedAnchors (namePlan env))
        body =
            Map.findWithDefault
                (local ++ "body")
                name
                (plannedBodies (namePlan env))
        bodyType =
            O.forallT
                sourceBinders
                (monadicType (thunkType ty))
        allocation = suspendPayload ty action
        declarations =
            if null scoped
                then []
                else [O.sig body bodyType, O.val body [] allocation]
        producer = if null scoped then allocation else O.v body
        signature = case payload of
            Payload route variables source _ ->
                O.forallT
                    <$> payloadBinders route env variables
                    <*> (thunkType <$> renderType (sourceDatatypes env) source)
    sharedType <- signature
    pure
        ( O.letE declarations $
            O.doE
                [O.bind root producer]
                ( O.letE
                    [ O.sig local sharedType
                    , O.val local [] (payloadView ty (O.v root))
                    ]
                    continuation
                )
        )
