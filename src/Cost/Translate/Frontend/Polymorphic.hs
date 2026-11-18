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
import Cost.Translate.Output qualified as O
import Cost.Translate.Sharing (nativeSharing, sharingType)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC
import GHC.Core.DataCon (dataConInstOrigArgTys)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.TyCon (isAlgTyCon)
import GHC.Core.Type
    ( isLiftedTypeKind
    , splitTyConApp_maybe
    , tyVarKind
    )
import GHC.Types.Name (getOccString)
import GHC.Types.Unique.Set (elementOfUniqSet)

data Payload = Payload [TyVar] Type PayloadBody
data PayloadBody
    = ConstructorPayload String [(String, Field)]
    | ConditionalPayload (LHsExpr GhcRn) Payload Payload
data Field = Ordinary (LHsExpr GhcRn) | Generalized Payload

constructorPayload ::
    Env ->
    Name ->
    Type ->
    Match GhcRn (LHsExpr GhcRn) ->
    Maybe ([Name], Payload)
constructorPayload env name ty match = do
    case nativeSharing (sourceDatatypes env) Map.empty ty of
        Right Nothing -> pure ()
        _ -> Nothing
    let renamings = bodyTypeRenamings Map.empty env name (O.tuple []) ty
        (variables, body) = sharingType renamings ty
        scoped = Map.findWithDefault [] name (scopedTypeVariables env)
        rhs = m_grhss match
    guard (not (null variables) && all (isLiftedTypeKind . tyVarKind) variables)
    case (grhssLocalBinds rhs, grhssGRHSs rhs) of
        (EmptyLocalBinds _, [L _ (GRHS _ [] expression)]) ->
            (scoped,) <$> planPayload env variables body expression
        _ -> Nothing

planPayload :: Env -> [TyVar] -> Type -> LHsExpr GhcRn -> Maybe Payload
planPayload env variables ty expression = do
    let (headExpression, arguments) = spine expression
    body <- case unLoc headExpression of
        HsIf _ condition yes no
            | null arguments ->
                ConditionalPayload condition
                    <$> planPayload env variables ty yes
                    <*> planPayload env variables ty no
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

payloadType :: Env -> Payload -> Either String O.Ty
payloadType env (Payload variables ty _) =
    O.forallT (map (O.b . getOccString) variables)
        <$> renderType (sourceDatatypes env) ty

typedOperation :: String -> O.Ty -> O.Expr
typedOperation name = O.typeApp (O.typeApp (O.v name) (O.t "M.MemoristM"))

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
                ( [O.bind name (O.app (typedOperation "M.lazy" ty) [action])]
                , payloadView ty (O.v name)
                )

allocatePayload ::
    (LHsExpr GhcRn -> Either String O.Expr) ->
    Env ->
    Name ->
    ([Name], Payload) ->
    O.Expr ->
    Either String O.Expr
allocatePayload _ env name _ continuation
    | let used = usedNames continuation
    , localName env name `Set.notMember` used =
        pure continuation
allocatePayload emit env name (scoped, payload) continuation = do
    ty <- payloadType env payload
    action <- emitPayload emit env payload
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
                [O.bk (getOccString variable) (O.t "Type") | variable <- scoped]
                (O.tc "M.MemoristM" [O.tc "M.Th" [ty]])
        allocation = O.app (typedOperation "M.lazy" ty) [action]
        declarations =
            if null scoped
                then []
                else [O.sig body bodyType, O.val body [] allocation]
        producer = if null scoped then allocation else O.v body
        signature = case payload of
            Payload variables source _ ->
                O.forallT (map (O.b . getOccString) variables)
                    . O.tc "M.Th"
                    . (: [])
                    <$> renderType (sourceDatatypes env) source
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
