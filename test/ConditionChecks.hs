module ConditionChecks (checkConditionOutput) where

import Control.Monad (unless, when)
import Data.Data (Data, cast, gmapQ)
import Data.Maybe (maybeToList)
import GHC (GhcPs, HsExpr(..), LHsExpr, Match(..), StmtLR(..), unLoc)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)

-- Inspect the same published bindings that the reference probes exercise.
checkConditionOutput :: (String -> IO (Match GhcPs (LHsExpr GhcPs))) -> IO ()
checkConditionOutput binding = do
    guarded <- rhs "guardedM"
    checkComparison "guardedM" guarded
    when (any failureName guarded) $
        fail "guardedM: exhaustive otherwise guard retained a failure path"
    rhs "prefixIfM" >>= checkComparison "prefixIfM"
    chain <- rhs "threeWayM"
    let localActions = [() | HsDo _ _ ss <- chain, s <- unLoc ss, LetStmt {} <- [unLoc s]]
    unless (length [() | HsIf {} <- chain] == 2 &&
            null localActions && null [() | HsLet {} <- chain]) $
        fail "threeWayM: two ordered comparisons should branch directly into their alternatives"
    known <- rhs "knownM"
    unless (null [() | HsIf {} <- known] && not (any failureName known)) $
        fail "knownM: literal guards and if conditions retained an unreachable branch"
  where
    rhs name = expressions . m_grhss <$> binding name

checkComparison :: String -> [HsExpr GhcPs] -> IO ()
checkComparison name body = do
    unless (any directComparison [test | HsIf _ test _ _ <- body]) $
        fail (name ++ ": expected (<=) directly in the if condition")
    when (any appliedLambda body) $
        fail (name ++ ": saturated primitive retained an immediately applied lambda")

-- Accept either spelling of the operator in the artifact, independently of
-- whether the source used it infix or prefix.
directComparison :: LHsExpr GhcPs -> Bool
directComparison test = case unwrapped test of
    OpApp _ _ operator _ -> named "<=" (unwrapped operator)
    HsApp _ partial _ | HsApp _ operator _ <- unwrapped partial ->
        named "<=" (unwrapped operator)
    _ -> False

appliedLambda :: HsExpr GhcPs -> Bool
appliedLambda (HsApp _ function _) = isLambda function
appliedLambda (OpApp _ function operator _) = named "$" (unwrapped operator) && isLambda function
appliedLambda _ = False

isLambda :: LHsExpr GhcPs -> Bool
isLambda expression = case unwrapped expression of
    HsLam {} -> True
    _ -> False

unwrapped :: LHsExpr GhcPs -> HsExpr GhcPs
unwrapped expression = case unLoc expression of
    HsPar _ body -> unwrapped body
    ExprWithTySig _ body _ -> unwrapped body
    other -> other

failureName :: HsExpr GhcPs -> Bool
failureName e = named "error" e || named "undefined" e

named :: String -> HsExpr GhcPs -> Bool
named expected (HsVar _ n) = occNameString (rdrNameOcc (unLoc n)) == expected
named _ _ = False

expressions :: Data a => a -> [HsExpr GhcPs]
expressions tree = maybeToList (cast tree) ++ concat (gmapQ expressions tree)
