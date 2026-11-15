module Main where
import Generated
import qualified Example as Source
import Memorist.Memorist
import qualified Memorist.Base.Thunk as MT
translated :: MemoristM Int
translated = do { b <- initialize; i <- fromPure1 (oneM b) (0::Int); j <- fromPure1 (twoM b) (0::Int); pure (i+j) }
main = do
  if (Source.one 0 + Source.two 0) == (2 :: Int) then pure () else error "source oracle failed"
  print (correct translated (2 :: Int),
    MT.toInteger (infcost translated fullDemand))
