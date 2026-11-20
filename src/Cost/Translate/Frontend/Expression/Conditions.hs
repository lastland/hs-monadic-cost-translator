module Cost.Translate.Frontend.Expression.Conditions
    ( constantCondition
    , underCondition
    ) where

import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.Expression.Scalar
    ( ScalarCall (..)
    , checkSignature
    , scalarCall
    )
import Cost.Translate.Frontend.Expression.Thunks
    ( conditionalAction
    , emitSaturated
    )
import Cost.Translate.Frontend.Expression.Values (withBoolOperand, withScalar)
import Cost.Translate.Frontend.Match.Fallback (withFallback)
import Cost.Translate.Frontend.NamePlan (plannedOperands)
import Cost.Translate.Frontend.Resolve (booleanConstant)
import Cost.Translate.Frontend.Syntax (argumentKey)
import Cost.Translate.Output qualified as O
import Cost.Translate.Primitive (Destination (Branching), Primitive (..))
import Data.Map.Strict qualified as Map
import GHC
import GHC.Builtin.Types (boolTy)

type Result = Either String O.Expr
type Branch = Env -> Result
type Choose = Branch -> Branch -> Result
type Continue = Env -> Choose -> Result

select :: Env -> (O.Expr -> O.Expr -> Result) -> Choose
select env build yes no = do
    y <- yes env
    n <- no env
    build y n

constantCondition :: Env -> LHsExpr GhcRn -> Maybe Bool
constantCondition env expr = case unLoc expr of
    HsPar _ e -> constantCondition env e
    ExprWithTySig _ e signature -> do
        checkSignature env expr signature boolTy
        constantCondition env e
    HsVar _ name -> booleanConstant (unLoc name)
    _ -> Nothing

underCondition ::
    (Env -> String -> LHsExpr GhcRn -> Either String ([O.Stmt], O.Expr)) ->
    (Env -> LHsExpr GhcRn -> Result) ->
    Env ->
    LHsExpr GhcRn ->
    Continue ->
    Result
underCondition prepare render = go Nothing
    where
        go hint env condition continue = case unLoc condition of
            HsPar _ expr -> go hint env expr continue
            _
                | Just b <- constantCondition env condition ->
                    continue env (select env (constant b))
            _
                | Just call <- scalarCall env condition
                , Just swap <- order (scalarOperation call) ->
                    short env call swap continue
            _
                | Just result <- scalar branch -> result
                | Just (x, a) <- hint ->
                    withBoolOperand prepare env x a condition branch
                | otherwise -> continue env (select env existing)
            where
                scalar = withScalar prepare env condition
                branch scope value = continue scope (select scope build)
                    where
                        build y n = Right (O.ifE value y n)
                existing yes no
                    | Just act <- saturated (Branching yes no) = act
                    | otherwise = do
                        act <- render env condition
                        pure (conditionalAction act yes no)
                saturated = emitSaturated env (prepare env) condition
        constant b y n = Right (if b then y else n)
        order Conjunction = Just id
        order Disjunction = Just flip
        order _ = Nothing
        short env call swap continue =
            case (argumentNames env site 2, names) of
                ([x, y], Just (a, b)) ->
                    left (Just (x, a)) $ \common chooseLeft ->
                        continue common $ swap $ \taken skipped -> do
                            fallback <- skipped common
                            withFallback x fallback $ \miss ->
                                let skip = const (Right miss)
                                    right s = operand (Just (y, b)) s $
                                        \_ choose -> swap choose taken skip
                                 in swap chooseLeft right skip
                _ -> Left "strict scalar argument layout mismatch"
            where
                site = argumentKey (scalarSite call)
                names = Map.lookup site (plannedOperands (namePlan env))
                left hint = go hint env (scalarLeft call)
                operand hint s = go hint s (scalarRight call)
