module Main where
import Generated
import qualified Example as Source
import Memorist.Memorist
import qualified Memorist.Base.Thunk as MT
translated :: MemoristM Int
translated = fromPure1 partialM (5 :: Int)
main = do
  if (Source.partial (5 :: Int)) == (25 :: Int) then pure () else error "source oracle failed"
  print (correct translated (25 :: Int),
    MT.toInteger (infcost translated fullDemand))
