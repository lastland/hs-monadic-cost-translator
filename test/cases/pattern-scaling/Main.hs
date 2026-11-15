module Main where
import Generated
import qualified Example as S
import Memorist.Memorist
import qualified Memorist.Base.Thunk as MT
import Control.Monad (unless)
main = mapM_ check [(a,b) | a <- [-1..11::Int], b <- [-1..11::Int]]
check (a,b) = unless (correct (fromPure2 chooseM a b) (S.choose a b :: Int) && MT.toInteger (infcost (fromPure2 chooseM a b :: MemoristM Int) fullDemand) == 1) (fail (show (a,b)))
