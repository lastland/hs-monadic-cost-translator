module Cost.Translate.Frontend.Expression.Arguments
    ( fieldThunk
    , applyArguments
    ) where

import Control.Monad (zipWithM)
import Cost.Translate.Callable (spine)
import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.Expression.Thunks
    ( lazyArguments
    , reuseThunk
    , suspendArgument
    )
import Cost.Translate.Frontend.Expression.Values (pureScalar)
import Cost.Translate.Frontend.Resolve (resolve)
import Cost.Translate.Frontend.Syntax (argumentKey)
import Cost.Translate.Output qualified as O
import Data.Maybe (isNothing)
import GHC

type Argument = ([O.Stmt], O.Expr)
type Prepare = String -> LHsExpr GhcRn -> Either String Argument

prepareArguments ::
    Env ->
    Prepare ->
    LHsExpr GhcRn ->
    [LHsExpr GhcRn] ->
    Either String [Argument]
prepareArguments env prepare site args =
    zipWithM prepare (argumentNames env (argumentKey site) (length args)) args

applyArguments ::
    Env ->
    Prepare ->
    LHsExpr GhcRn ->
    ([O.Expr] -> O.Expr) ->
    [LHsExpr GhcRn] ->
    Either String O.Expr
applyArguments env prepare site finish args =
    (`lazyArguments` finish) <$> prepareArguments env prepare site args

fieldThunk ::
    Env ->
    Prepare ->
    String ->
    LHsExpr GhcRn ->
    Either String Argument
fieldThunk env prepare field expr = case delayedCall env expr of
    Just (target, args)
        | let names = argumentNames env (argumentKey (bare expr)) (length args)
        , let plans = zipWith plan names args
        , any (\(outside, _) -> not (null outside)) plans -> do
            inside <- traverse snd plans
            let call = O.call target (map snd inside)
                action = O.doE (concatMap fst inside) call
                (bind, ref) = suspendArgument field action
            pure (concatMap fst plans ++ bind, ref)
    _ -> prepare field expr
    where
        plan n arg
            | isNothing (reuseThunk env arg)
            , Just value <- pureScalar env arg =
                let (bind, ref) = suspendArgument n (O.ret value)
                 in (bind, Right ([], ref))
            | otherwise = ([], prepare n arg)

delayedCall :: Env -> LHsExpr GhcRn -> Maybe (String, [LHsExpr GhcRn])
delayedCall env expr = do
    (fun, args) <- case unLoc (bare expr) of
        HsApp {} -> Just (spine expr)
        OpApp _ left op right -> Just (op, [left, right])
        _ -> Nothing
    HsVar _ name <- pure (unLoc fun)
    Function target arity <- resolve env (unLoc name)
    if length args == arity then Just (target, args) else Nothing

bare :: LHsExpr GhcRn -> LHsExpr GhcRn
bare expr = case unLoc expr of
    HsPar _ nested -> bare nested
    _ -> expr
