module Main where
import Generated
import qualified Example as Source
import Memorist.Memorist
import qualified Memorist.Base.Thunk as MT
translated :: MemoristM Int
translated = fromPure1 observedM (undefined :: Int)
main = do
  if (Source.observed (undefined :: Int)) == (1 :: Int) then pure () else error "source oracle failed"
  print (correct translated (1 :: Int),
    MT.toInteger (infcost translated fullDemand))
