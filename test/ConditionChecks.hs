module ConditionChecks (checkConditionOutput) where

import Control.Monad (unless, when)
import Data.List (unsnoc)
import GHC (GhcPs, HsDoFlavour(..), HsExpr(..), Match(..), StmtLR(..), unLoc)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GeneratedSyntax

-- Inspect the same published bindings that the reference probes exercise.
checkConditionOutput :: (String -> IO Binding) -> IO ()
checkConditionOutput binding = do
    guarded <- binding "guardedM"
    checkComparison "guardedM" guarded
    when (any failureName (expressions guarded)) $
        fail "guardedM: exhaustive otherwise guard retained a failure path"
    binding "prefixIfM" >>= checkComparison "prefixIfM"
    chain <- binding "threeWayM"
    let tree = expressions chain
        branches = [() | HsIf{} <- tree]
        localActions = [() | HsDo _ _ ss <- tree, s <- unLoc ss, LetStmt{} <- [unLoc s]]
        ordered = do
            (first, _, remaining) <- oneRhs chain >>= tailIf
            (second, _, _) <- tailIf remaining
            pure (comparison "<" first && comparison "==" second)
    unless (length branches == 2 && null localActions && null [() | HsLet{} <- tree] &&
            ordered == Just True) $
        fail "threeWayM: two ordered comparisons should branch directly into their alternatives"
    known <- expressions <$> binding "knownM"
    unless (null [() | HsIf {} <- known] && not (any failureName known)) $
        fail "knownM: literal guards and if conditions retained an unreachable branch"

checkComparison :: String -> Binding -> IO ()
checkComparison name binding = do
    case oneRhs binding >>= tailIf of
        Just (test, _, _) | comparison "<=" test -> pure ()
        _ -> fail (name ++ ": expected (<=) directly in the if condition")
    when (any appliedLambda (expressions binding)) $
        fail (name ++ ": saturated primitive retained an immediately applied lambda")

comparison :: String -> Expr -> Bool
comparison operator test = case arguments operator test of
    Just [_, _] -> True
    _ -> False

-- Follow the actual continuation. A comparison in an uncalled local action
-- cannot supply the second test; a local let around a direct test is harmless.
tailIf :: Expr -> Maybe (Expr, Expr, Expr)
tailIf expression = case bareExpression expression of
    HsIf _ test yes no -> Just (test, yes, no)
    HsDo _ (DoExpr Nothing) statements -> do
        (_, terminal) <- unsnoc (unLoc statements)
        tailIf =<< terminalExpression terminal
    HsLet _ _ body -> tailIf body
    _ | Just [value] <- arguments "pure" expression -> tailIf value
    _ -> Nothing

appliedLambda :: HsExpr GhcPs -> Bool
appliedLambda (HsApp _ function _) = isLambda function
appliedLambda (OpApp _ function operator _) =
    maybe False (named "$") (expressionVariable operator) && isLambda function
appliedLambda _ = False

isLambda :: Expr -> Bool
isLambda expression = case bareExpression expression of
    HsLam {} -> True
    _ -> False

failureName :: HsExpr GhcPs -> Bool
failureName (HsVar _ n) = occNameString (rdrNameOcc (unLoc n)) `elem` ["error", "undefined"]
failureName _ = False

expressions :: Binding -> [HsExpr GhcPs]
expressions = collectAst . m_grhss
