module Main where
import Generated
import qualified Example as S
import Memorist.Memorist hiding (toInteger)
import Memorist.Data.List
import qualified Memorist.Base.Thunk as MT
import Test.QuickCheck
import Control.Monad (unless)
propertyFor :: [Int] -> Property
propertyFor xs = conjoin [ correct (fromPure1 mappedM xs) (S.mapped xs) === True, correct (fromPure1 foldedM xs) (S.folded xs) === True, MT.toInteger (infcost (fromPure1 mappedM xs) fullDemand) === (2 * toInteger (length xs) + 2), MT.toInteger (infcost (fromPure1 foldedM xs) fullDemand) === (2 * toInteger (length xs) + 2)]
main = do
  result <- quickCheckResult propertyFor
  unless (isSuccess result) (fail "generated value/cost property")
  let stream = fromPure1 streamM (5::Int)
  unless (take 3 (abstract (value (eval1 stream))) == take 3 (S.stream (5::Int))) (fail "finite infinite values")
  unless (MT.toInteger (infcost' stream (ConsD (Need 5) (Need (ConsD (Need 6) (Need (ConsD (Need 7) Skip)))))) == 3) (fail "finite infinite cost")
