module Main where
import Generated
import qualified Example as Source
import Memorist.Memorist
import qualified Memorist.Base.Thunk as MT
translated :: MemoristM Int
translated = fromPure1 composedM (5 :: Int)
main = do
  if (Source.composed (5 :: Int)) == (20 :: Int) then pure () else error "source oracle failed"
  print (correct translated (20 :: Int),
    MT.toInteger (infcost translated fullDemand))
