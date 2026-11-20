module Cost.Translate.Frontend.Expression.Conditions
    ( constantCondition
    , returningCondition
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
import Cost.Translate.Frontend.Expression.Values
    ( returnBoolOperand
    , withBoolOperand
    , withScalar
    )
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
type Prepare =
    Env -> String -> LHsExpr GhcRn -> Either String ([O.Stmt], O.Expr)
type Render = Env -> LHsExpr GhcRn -> Result

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
    _ -> do
        (takeLeft, call) <- shortCall env expr
        let result child = constantCondition env (child call)
        case result scalarLeft of
            Just b | b == takeLeft -> result scalarRight
            Just b -> Just b
            Nothing
                | result scalarRight == Just (not takeLeft) ->
                    Just (not takeLeft)
            _ -> Nothing

shortCall :: Env -> LHsExpr GhcRn -> Maybe (Bool, ScalarCall)
shortCall env expr = do
    call <- scalarCall env expr
    case scalarOperation call of
        Conjunction -> Just (True, call)
        Disjunction -> Just (False, call)
        _ -> Nothing

returningCondition :: Prepare -> Render -> Env -> LHsExpr GhcRn -> Maybe Result
returningCondition prepare render = emit
    where
        emit env expr = do
            (taking, call) <- shortCall env expr
            pure $ operands env call $ \(x, a) (y, _) ->
                let lhs = scalarLeft call
                    rhs = scalarRight call
                    left = underOperand prepare render (Just (x, a)) env lhs
                    right s = case emit s rhs of
                        Just result -> result
                        Nothing -> returnBoolOperand prepare s y rhs
                    skip _ = Right (O.ret (O.v (show (not taking))))
                 in left $ \_ choose ->
                        if taking then choose right skip else choose skip right

underCondition ::
    Prepare -> Render -> Env -> LHsExpr GhcRn -> Continue -> Result
underCondition prepare render = underOperand prepare render Nothing

underOperand :: Prepare -> Render -> Operand
underOperand prepare render = go
    where
        go hint env condition continue = case unLoc condition of
            HsPar _ expr -> go hint env expr continue
            _
                | Just (b, call) <- shortCall env condition ->
                    short render go env call b continue
            _
                | Just b <- constantCondition env condition ->
                    continue env (fixed env b)
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

fixed :: Env -> Bool -> Choose
fixed env b = select env (\y n -> Right (if b then y else n))

type Operand =
    Maybe (String, String) -> Env -> LHsExpr GhcRn -> Continue -> Result

short :: Render -> Operand -> Env -> ScalarCall -> Bool -> Continue -> Result
short render operand env call taking continue =
    operands env call $ \(x, a) (y, b) ->
        operand (Just (x, a)) env lhs $ \common choose ->
            let left = order choose
                right s = operand (Just (y, b)) s rhs
                dead _ = Right (O.tuple [])
                after s = continue s (fixed s (not taking))
                run s k = right s $ \_ pick ->
                    if rightTakes
                        then order pick k dead
                        else order pick dead k
             in case constantCondition env lhs of
                    Just v | v == taking -> left (`right` continue) dead
                    Just _ -> render common rhs >> left dead after
                    Nothing
                        | Just _ <- rightResult
                        , rightTakes ->
                            continue common $ order $ \taken skipped ->
                                left (`run` taken) skipped
                    Nothing | Just _ <- rightResult -> do
                        end <- after common
                        withFallback x end $ \miss ->
                            let skip = const (Right miss)
                             in left (`run` skip) skip
                    Nothing -> continue common $ order $ \yes no -> do
                        fallback <- no common
                        withFallback x fallback $ \miss ->
                            let skip = const (Right miss)
                                test s = right s $ \_ pick ->
                                    order pick yes skip
                             in left test skip
    where
        order f = if taking then f else flip f
        lhs = scalarLeft call
        rhs = scalarRight call
        rightResult = constantCondition env rhs
        rightTakes = rightResult == Just taking

operands ::
    Env ->
    ScalarCall ->
    ((String, String) -> (String, String) -> Result) ->
    Result
operands env call build =
    case (argumentNames env site 2, values) of
        ([x, y], Just (a, b)) -> build (x, a) (y, b)
        _ -> Left "strict scalar argument layout mismatch"
    where
        site = argumentKey (scalarSite call)
        values = Map.lookup site (plannedOperands (namePlan env))
