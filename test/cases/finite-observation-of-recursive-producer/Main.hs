module Main where
import Generated
import qualified Example as Source
import Memorist.Memorist
import qualified Memorist.Base.Thunk as MT
translated :: MemoristM Int
translated = fromPure1 observeM (5 :: Int)
main = do
  if (Source.observe (5 :: Int)) == (6 :: Int) then pure () else error "source oracle failed"
  print (correct translated (6 :: Int),
    MT.toInteger (infcost translated fullDemand))
