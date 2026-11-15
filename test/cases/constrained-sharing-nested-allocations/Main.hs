module Main where
import Generated
import qualified Example as Source
import Memorist.Memorist
import qualified Memorist.Base.Thunk as MT
translated :: MemoristM Int
translated = do { b <- initialize; i <- fromPure1 (asIntM b) (0::Int); j <- fromPure1 (asIntegerM b) (0::Integer); pure (if (i,j) == (1,2) then 12 else 0) }
main = do
  if (if (Source.asInt 0, Source.asInteger 0) == (1,2) then 12 else 0) == (12 :: Int) then pure () else error "source oracle failed"
  print (correct translated (12 :: Int),
    MT.toInteger (infcost translated fullDemand))
