module Main where
import Generated
import qualified Example as Source
import Memorist.Memorist
import qualified Memorist.Base.Thunk as MT
translated :: MemoristM Int
translated = fromPure1 boxedM (5 :: Int)
main = do
  if (Source.boxed (5 :: Int)) == (5 :: Int) then pure () else error "source oracle failed"
  print (correct translated (5 :: Int),
    MT.toInteger (infcost translated fullDemand))
