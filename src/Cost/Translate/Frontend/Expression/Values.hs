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
import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.Expression.Scalar
import Cost.Translate.Frontend.NamePlan (plannedActions, plannedOperands)
import Cost.Translate.Frontend.Resolve (resolveAt)
import Cost.Translate.Frontend.Syntax (actionKey, argumentKey)
import Cost.Translate.Output qualified as O
import Cost.Translate.Primitive (Primitive (..))
import Cost.Translate.Typed (hasFreshAnnotationVariables, sourceKey)
import Data.List (find)
import Data.Map.Strict qualified as Map
import GHC
import GHC.Builtin.Types (boolTy, charTy, unitDataCon, unitTy)
import GHC.Core.TyCo.Compare (eqType)

type Expression = LHsExpr GhcRn
type Prepare = Env -> String -> Expression -> Either String ([O.Stmt], O.Expr)
type Branch = Env -> O.Expr -> Either String O.Expr

data Consumer = Returning | Continuing Branch
data ScalarValue = Ready O.Expr | Action [O.Stmt] O.Expr (O.Expr -> Env)

consume :: Consumer -> Env -> String -> ScalarValue -> Either String O.Expr
consume Returning _ _ (Ready value) = Right (O.ret value)
consume Returning _ _ (Action before action _) = Right (O.doE before action)
consume (Continuing branch) env _ (Ready value) = branch env value
consume (Continuing branch) _ name (Action before action scope) =
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
emitBoolCondition env prepare expression branch = case unLoc expression of
    HsPar _ nested -> emitBoolCondition env prepare nested branch
    HsVar {} -> do
        (_, ty) <- reference env expression
        guard (eqType ty boolTy)
        let site = actionKey "condition" (getLocA expression)
        name <- Map.lookup site (plannedActions (namePlan env))
        pure (withBoolOperand prepare env name name expression branch)
    _ -> Nothing

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
    consume (Continuing branch) env valueName scoped

reference :: Env -> Expression -> Maybe (Name, Type)
reference env expression = case unLoc expression of
    HsPar _ nested -> reference env nested
    ExprWithTySig _ nested _ -> reference env nested
    HsVar _ located -> do
        let name = unLoc located
            key = sourceKey (getLocA expression)
        Thunk _ <- resolveAt env expression name
        (ty, _) <- Map.lookup key (inferredExpressions env)
        pure (name, ty)
    _ -> Nothing

knownValue :: Env -> Expression -> Maybe (Type, O.Expr)
knownValue env expression = do
    (name, ty) <- reference env expression
    entries <- Map.lookup name (knownScalarValues env)
    find (eqType ty . fst) entries

remember :: Env -> Type -> Expression -> O.Expr -> Env
remember env expected expression value = case reference env expression of
    Just (name, ty)
        | eqType ty expected ->
            let facts' = Map.insertWith (++) name [(ty, value)] facts
             in env {knownScalarValues = facts'}
    _ -> env
    where
        facts = knownScalarValues env

operand ::
    Prepare ->
    Env ->
    Type ->
    String ->
    Expression ->
    Either String ScalarValue
operand prepare env expected thunkName expression
    | Just value <- valueAt env expected expression = Right (Ready value)
    | otherwise = do
        (before, thunk) <- prepare env thunkName expression
        let action = O.call "M.force" [thunk]
        pure (Action before action (remember env expected expression))

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
                            rhs <- consume Returning scope b right
                            let action = Action [] (choose rhs) (const scope)
                            consume consumer scope b action
                        strict op inner rhs =
                            consume consumer inner b (Ready (O.op op lhs rhs))
                    case scalarOperation call of
                        StrictOperator op ->
                            consume (Continuing (strict op)) scope b right
                        Conjunction -> short (\r -> O.ifE lhs r (bool False))
                        Disjunction -> short (O.ifE lhs (bool True))
            consume (Continuing next) env a left
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
        StrictOperator op <- pure (scalarOperation call)
        let value = valueAt env (scalarOperandType call)
        lhs <- value (scalarLeft call)
        rhs <- value (scalarRight call)
        pure (scalarResultType call, O.op op lhs rhs)
    where
        site = sourceKey (getLocA expression)
        literal ty = (ty,) <$> scalarLiteral env ty expression
