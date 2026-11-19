module Cost.Translate.Frontend.Polymorphic
    ( constructorPayload
    , allocatePayload
    ) where

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
import Cost.Translate.Frontend.Syntax (argumentKey)
import Cost.Translate.Kinds (renderBinder)
import Cost.Translate.Output qualified as O
import Cost.Translate.Sharing (nativeSharing, sharingType)
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

data Payload = Payload [TyVar] Type PayloadBody
data PayloadBody
    = ConstructorPayload String [(String, Field)]
    | ConditionalPayload (LHsExpr GhcRn) Payload Payload
    | ApplicationPayload String String Payload
data Field = Ordinary (LHsExpr GhcRn) | Generalized Payload

constructorPayload ::
    Env ->
    Name ->
    Type ->
    Match GhcRn (LHsExpr GhcRn) ->
    Maybe ([TyVar], Payload)
constructorPayload env name ty match = do
    case nativeSharing (plannedNativeSchemas (namePlan env)) Map.empty ty of
        Right Nothing -> pure ()
        _ -> Nothing
    let renamings = bodyTypeRenamings Map.empty env name (O.tuple []) ty
        (variables, body) = sharingType renamings ty
        rhs = m_grhss match
    guard (not (null variables))
    scoped <- payloadScope env name ty
    _ <- either (const Nothing) Just (payloadBinders env variables)
    case (grhssLocalBinds rhs, grhssGRHSs rhs) of
        (EmptyLocalBinds _, [L _ (GRHS _ [] expression)]) ->
            (scoped,) <$> planPayload env variables body expression
        _ -> Nothing

payloadScope :: Env -> Name -> Type -> Maybe [TyVar]
payloadScope env name ty
    | null names = Just []
    | otherwise = do
        guard (map (getName . binderVar) explicit == names)
        _ <- either (const Nothing) Just (payloadBinders env original)
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

planPayload :: Env -> [TyVar] -> Type -> LHsExpr GhcRn -> Maybe Payload
planPayload env variables ty expression = do
    let (headExpression, arguments) = spine expression
    body <- case unLoc headExpression of
        HsIf _ condition yes no
            | null arguments ->
                ConditionalPayload condition
                    <$> planPayload env variables ty yes
                    <*> planPayload env variables ty no
        HsVar _ located
            | [argument] <- arguments
            , Just target <- payloadFunction env (unLoc located) -> do
                [slot] <- pure (argumentNames env (argumentKey expression) 1)
                ApplicationPayload target slot
                    <$> planPayload env variables ty argument
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
    pure (Payload variables ty body)
    where
        field ty' argument =
            case filter dependent variables of
                [] -> Just (Ordinary argument)
                quantified ->
                    Generalized <$> planPayload env quantified ty' argument
            where
                dependent variable =
                    variable `elementOfUniqSet` tyCoVarsOfType ty'

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
payloadType env (Payload variables ty _) =
    O.forallT
        <$> payloadBinders env variables
        <*> renderType (sourceDatatypes env) ty

payloadBinders :: Env -> [TyVar] -> Either String [O.Binder]
payloadBinders env variables = do
    guardNames
    mapM binder variables
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
        binder variable
            | isLiftedTypeKind (tyVarKind variable) =
                pure (O.b (getOccString variable))
            | otherwise =
                renderBinder
                    (sourceDatatypes env)
                    (setTyVarKind variable (kinds (tyVarKind variable)))

typedOperation :: String -> O.Ty -> O.Expr
typedOperation name = O.typeApp (O.typeApp (O.v name) (O.t "M.MemoristM"))

suspendPayload :: O.Ty -> O.Expr -> O.Expr
suspendPayload ty action = O.app (typedOperation "M.lazy" ty) [action]

payloadView :: O.Ty -> O.Expr -> O.Expr
payloadView ty thunk =
    O.app
        (O.typeApp (O.v "mapThunk") ty)
        [O.lam [O.p "payload"] (O.v "payload"), thunk]

emitPayload ::
    (LHsExpr GhcRn -> Either String O.Expr) ->
    Env ->
    Payload ->
    Either String O.Expr
emitPayload emit env payload@(Payload _ _ body) = case body of
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
allocatePayload emit env name (_, payload) continuation
    | let used = usedNames continuation
    , localName env name `Set.notMember` used =
        emitPayload emit env payload >> pure continuation
allocatePayload emit env name (scoped, payload) continuation = do
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
                (O.tc "M.MemoristM" [O.tc "M.Th" [ty]])
        allocation = suspendPayload ty action
        declarations =
            if null scoped
                then []
                else [O.sig body bodyType, O.val body [] allocation]
        producer = if null scoped then allocation else O.v body
        signature = case payload of
            Payload variables source _ ->
                O.forallT
                    <$> payloadBinders env variables
                    <*> ( O.tc "M.Th" . (: [])
                            <$> renderType (sourceDatatypes env) source
                        )
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
