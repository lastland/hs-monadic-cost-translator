module Main where
import Generated
import qualified Example as Source
import Memorist.Memorist
import qualified Memorist.Base.Thunk as MT
translated :: MemoristM Int
translated = do { bindings <- initialize; b <- fromPure1 (boolsM bindings) True; if b then fromPure1 (intsM bindings) (5::Int) else pure 0 }
main = do
  if (Source.ints (5::Int)) == (5 :: Int) then pure () else error "source oracle failed"
  print (correct translated (5 :: Int),
    MT.toInteger (infcost translated fullDemand))
