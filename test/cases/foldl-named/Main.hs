module Main where
import Generated
import qualified Example as Source
import Memorist.Memorist
import qualified Memorist.Base.Thunk as MT
translated :: MemoristM Int
translated = fromPure1 foldedM (5 :: Int)
main = do
  if (Source.folded (5 :: Int)) == (10 :: Int) then pure () else error "source oracle failed"
  print (correct translated (10 :: Int),
    MT.toInteger (infcost translated fullDemand))
