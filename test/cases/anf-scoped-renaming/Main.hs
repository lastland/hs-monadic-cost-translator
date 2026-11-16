module Main where

import Control.Monad (unless)
import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import qualified Memorist.Base.Thunk as MT

check :: String -> Int -> M.MemoristM Int -> Int -> Integer -> IO ()
check label source program expectedValue expectedTicks = do
  unless (source == expectedValue) (fail (label ++ ": source value differs"))
  let generated = M.value (M.eval1 program)
      ticks = MT.toInteger (M.infcost program M.fullDemand)
  unless (generated == expectedValue) (fail (label ++ ": generated value differs"))
  unless (ticks == expectedTicks) (fail (label ++ ": entry ticks differ"))
  print (label, generated, ticks)

main :: IO ()
main = do
  -- Each observation starts with its own Bindings in the same evaluation.
  -- Parameter rows enter the outer function, inner, and ignore: three ticks.
  check "caseParameter/Int"
    (Source.caseParameter (undefined :: Int))
    (do bindings <- initialize
        M.fromPure1 (caseParameterM bindings) (undefined :: Int))
    1 3
  check "caseParameter/Bool"
    (Source.caseParameter (undefined :: Bool))
    (do bindings <- initialize
        M.fromPure1 (caseParameterM bindings) (undefined :: Bool))
    1 3
  check "guardParameter/Int"
    (Source.guardParameter (undefined :: Int))
    (do bindings <- initialize
        M.fromPure1 (guardParameterM bindings) (undefined :: Int))
    1 3
  check "guardParameter/Bool"
    (Source.guardParameter (undefined :: Bool))
    (do bindings <- initialize
        M.fromPure1 (guardParameterM bindings) (undefined :: Bool))
    1 3
  -- 2^64 wraps to zero at Int: outer + inner + one work entry = three.
  -- At Integer it is nonzero: outer + inner + two work entries = four.
  check "caseFactory/Int"
    (Source.caseFactory (0 :: Int))
    (do bindings <- initialize
        M.fromPure1 (caseFactoryM bindings) (0 :: Int))
    1 3
  check "caseFactory/Integer"
    (Source.caseFactory (0 :: Integer))
    (do bindings <- initialize
        M.fromPure1 (caseFactoryM bindings) (0 :: Integer))
    2 4
  check "guardFactory/Int"
    (Source.guardFactory (0 :: Int))
    (do bindings <- initialize
        M.fromPure1 (guardFactoryM bindings) (0 :: Int))
    1 3
  check "guardFactory/Integer"
    (Source.guardFactory (0 :: Integer))
    (do bindings <- initialize
        M.fromPure1 (guardFactoryM bindings) (0 :: Integer))
    2 4
