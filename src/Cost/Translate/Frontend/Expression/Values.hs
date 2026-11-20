module Cost.Translate.Frontend.Expression.Values
    ( knownValue
    , reference
    , pureScalar
    , returningScalar
    , withScalar
    , withBoolOperand
    ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Cost.Translate.Callable (spine)
import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.Expression.Scalar
import Cost.Translate.Frontend.Expression.Thunks (reuseThunk)
import Cost.Translate.Frontend.NamePlan (plannedActions, plannedOperands)
import Cost.Translate.Frontend.Resolve (resolveAt)
import Cost.Translate.Frontend.Syntax (actionKey, argumentKey)
import Cost.Translate.Output qualified as O
import Cost.Translate.Primitive (Primitive (..))
import Cost.Translate.Typed (hasFreshAnnotationVariables, sourceKey)
import Data.Map.Strict qualified as Map
import GHC
import GHC.Builtin.Types (boolTy, charTy, unitDataCon, unitTy)
import GHC.Core.TyCo.Compare (eqType)

type Expression = LHsExpr GhcRn
type Prepare = Env -> String -> Expression -> Either String ([O.Stmt], O.Expr)
type Branch = Env -> O.Expr -> Either String O.Expr

data Consumer = Returning | Continuing Branch
data ScalarValue
    = Ready [O.Stmt] Env O.Expr
    | Action [O.Stmt] O.Expr (O.Expr -> Env)

consume :: Consumer -> String -> ScalarValue -> Either String O.Expr
consume Returning _ (Ready before _ value) = Right (O.doE before (O.ret value))
consume Returning _ (Action before action _) = Right (O.doE before action)
consume (Continuing branch) _ (Ready before env value) =
    O.doE before <$> branch env value
consume (Continuing branch) name (Action before action scope) =
    O.doE (before ++ [O.bind name action]) <$> branch (scope value) value
    where
        value = O.v name

returningScalar :: Prepare -> Env -> Expression -> Maybe (Either String O.Expr)
returningScalar prep env expr = emitStrictPrimitive env prep expr Returning

withScalar ::
    Prepare ->
    Env ->
    Expression ->
    Branch ->
    Maybe (Either String O.Expr)
withScalar prepare env expr branch =
    emitBoolCondition env prepare expr branch
        <|> emitStrictPrimitive env prepare expr (Continuing branch)

emitBoolCondition ::
    Env ->
    Prepare ->
    Expression ->
    Branch ->
    Maybe (Either String O.Expr)
emitBoolCondition env prepare expression branch = do
    (_, ty) <- reference env expression
    guard (eqType ty boolTy)
    let (var, _) = spine expression
        site = actionKey "condition" (getLocA var)
    name <- Map.lookup site (plannedActions (namePlan env))
    pure (withBoolOperand prepare env name name var branch)

withBoolOperand ::
    Prepare ->
    Env ->
    String ->
    String ->
    Expression ->
    Branch ->
    Either String O.Expr
withBoolOperand prepare env thunkName valueName expr branch = do
    value <- operand prepare env boolTy thunkName expr
    let scoped = case value of
            Ready {} -> value
            Action before act scope -> Action [] (O.doE before act) scope
    consume (Continuing branch) valueName scoped

reference :: Env -> Expression -> Maybe (Name, Type)
reference env expression = case unLoc expression of
    HsPar _ nested -> reference env nested
    ExprWithTySig _ nested signature -> do
        pair@(_, ty) <- reference env nested
        checkSignature env expression signature ty
        pure pair
    HsVar _ located -> do
        let name = unLoc located
            key = sourceKey (getLocA expression)
        Thunk _ <- resolveAt env expression name
        (ty, _) <- Map.lookup key (inferredExpressions env)
        pure (name, ty)
    _ -> Nothing

knownValue :: Env -> Expression -> Maybe (Type, O.Expr)
knownValue env expression = do
    key@(_, ty) <- reference env expression
    (ty,) <$> knownScalar env key

remember :: Env -> Type -> Expression -> O.Expr -> Env
remember env expected expression value = case reference env expression of
    Just key@(_, ty)
        | eqType ty expected -> rememberScalar key value env
    _ -> env

operand ::
    Prepare ->
    Env ->
    Type ->
    String ->
    Expression ->
    Either String ScalarValue
operand prepare env expected thunkName expression
    | Just call <- scalarCall env expression
    , eqType expected (scalarResultType call)
    , Just (before, scope, value) <- strictTree env call =
        Right (Ready before scope value)
    | Just value <- valueAt env expected expression = Right (Ready [] env value)
    | otherwise = do
        (before, thunk) <- prepare env thunkName expression
        let action = O.call "M.force" [thunk]
        pure (Action before action (remember env expected expression))

strictTree :: Env -> ScalarCall -> Maybe ([O.Stmt], Env, O.Expr)
strictTree env call = do
    StrictOperator op <- pure (scalarOperation call)
    let site = argumentKey (scalarSite call)
        child = strictChild (scalarOperandType call)
    (a, b) <- Map.lookup site (plannedOperands (namePlan env))
    (left, inner, lhs) <- child env a (scalarLeft call)
    (right, outer, rhs) <- child inner b (scalarRight call)
    pure (left ++ right, outer, O.op op lhs rhs)
    where
        strictChild ty scope binder expr =
            case reference scope expr of
                Just (_, actual) | eqType ty actual ->
                    case knownValue scope expr of
                        Just (_, value) -> pure ([], scope, value)
                        Nothing -> do
                            thunk <- reuseThunk scope expr
                            let value = O.v binder
                                force = O.bind binder (O.call "M.force" [thunk])
                            pure ([force], remember scope ty expr value, value)
                _
                    | Just value <- scalarLiteral scope ty expr ->
                        pure ([], scope, value)
                _ -> do
                    nested <- scalarCall scope expr
                    guard (eqType ty (scalarResultType nested))
                    strictTree scope nested

emitStrictPrimitive ::
    Env ->
    Prepare ->
    Expression ->
    Consumer ->
    Maybe (Either String O.Expr)
emitStrictPrimitive env prepare expression consumer = do
    call <- scalarCall env expression
    let site = argumentKey (scalarSite call)
        ty = scalarOperandType call
        values = Map.lookup site (plannedOperands (namePlan env))
    pure $ case (argumentNames env site 2, values) of
        ([x, y], Just (a, b)) -> do
            left <- operand prepare env ty x (scalarLeft call)
            let next scope lhs = do
                    right <- operand prepare scope ty y (scalarRight call)
                    let short choose = do
                            rhs <- consume Returning b right
                            let action = Action [] (choose rhs) (const scope)
                            consume consumer b action
                        strict op inner rhs =
                            let value = O.op op lhs rhs
                             in consume consumer b (Ready [] inner value)
                    case scalarOperation call of
                        StrictOperator op ->
                            consume (Continuing (strict op)) b right
                        Conjunction -> short (\r -> O.ifE lhs r (bool False))
                        Disjunction -> short (O.ifE lhs (bool True))
            consume (Continuing next) a left
        _ -> Left "strict scalar argument layout mismatch"
    where
        bool = O.ret . O.v . show

pureScalar :: Env -> Expression -> Maybe O.Expr
pureScalar env = fmap snd . typedValue env

valueAt :: Env -> Type -> Expression -> Maybe O.Expr
valueAt env expected expression = do
    (actual, value) <- typedValue env expression
    guard (eqType expected actual)
    pure value

typedValue :: Env -> Expression -> Maybe (Type, O.Expr)
typedValue env expression = case unLoc expression of
    HsPar _ nested -> typedValue env nested
    ExprWithTySig _ nested signature -> do
        guard (not (hasFreshAnnotationVariables signature))
        (ty, _) <- Map.lookup site (inferredExpressions env)
        (ty,) <$> valueAt env ty nested
    HsOverLit {} -> Map.lookup site (scalarEvidence env) >>= literal
    HsLit {} -> literal charTy
    HsVar _ name | unLoc name == getName unitDataCon -> literal unitTy
    HsVar {} -> knownValue env expression <|> literal boolTy
    _ -> do
        call <- scalarCall env expression
        ([], _, value) <- strictTree env call
        pure (scalarResultType call, value)
    where
        site = sourceKey (getLocA expression)
        literal ty = (ty,) <$> scalarLiteral env ty expression
