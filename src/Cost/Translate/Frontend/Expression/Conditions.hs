module Cost.Translate.Frontend.Expression.Conditions (underCondition) where

import Cost.Translate.Frontend.Environment (Env)
import Cost.Translate.Frontend.Expression.Thunks
    ( conditionalAction
    , emitSaturated
    )
import Cost.Translate.Frontend.Expression.Values (withScalar)
import Cost.Translate.Frontend.Resolve (booleanConstant)
import Cost.Translate.Output qualified as O
import Cost.Translate.Primitive (Destination (Branching))
import GHC

type Result = Either String O.Expr
type Choose = O.Expr -> O.Expr -> Result

underCondition ::
    (Env -> String -> LHsExpr GhcRn -> Either String ([O.Stmt], O.Expr)) ->
    (Env -> LHsExpr GhcRn -> Result) ->
    Env ->
    LHsExpr GhcRn ->
    (Env -> Choose -> Result) ->
    Result
underCondition prepare render env condition continue = case unLoc condition of
    HsPar _ expr -> underCondition prepare render env expr continue
    HsVar _ name
        | Just b <- booleanConstant (unLoc name) ->
            continue env (\yes no -> Right (if b then yes else no))
    _ | Just result <- withScalar prepare env condition branch -> result
    _ -> continue env existing
    where
        branch scope value =
            continue scope (\yes no -> Right (O.ifE value yes no))
        existing yes no
            | Just act <- saturated (Branching yes no) = act
            | otherwise = do
                act <- render env condition
                pure (conditionalAction act yes no)
        saturated = emitSaturated env (prepare env) condition
