module Main where
import Generated
import qualified Example as S
import Memorist.Memorist
import qualified Memorist.Base.Thunk as MT
import Control.Exception
import Control.Monad (unless)
mustFail label x = do { outcome <- try (evaluate x) :: IO (Either SomeException Bool); case outcome of { Left _ -> pure (); Right _ -> fail label } }
main = do
  mustFail "source demanded failure" (S.demanded (5::Int) == 5)
  mustFail "generated demanded failure" (correct (fromPure1 demandedM (5::Int)) (5::Int))
  mustFail "source unmatched failure" (S.unmatched (5::Int) == 5)
  mustFail "generated unmatched failure" (correct (fromPure1 unmatchedM (5::Int)) (5::Int))
  unless (S.unused (5::Int) == 5 && correct (fromPure1 unusedM (5::Int)) (5::Int)) (fail "guard-first value")
  unless (MT.toInteger (infcost (fromPure1 unusedM (5::Int)) fullDemand) == 2) (fail "guard-first cost")
