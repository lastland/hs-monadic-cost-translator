module Main where
import Generated
import qualified Example as Source
import Memorist.Memorist
import qualified Memorist.Base.Thunk as MT
translated :: MemoristM Int
translated = do { b <- initialize; i <- fromPure1 (asListM b) (0::Int); j <- fromPure1 (asMaybeM b) (0::Int); pure (i+j) }
main = do
  if (Source.asList 0 + Source.asMaybe 0) == (12 :: Int) then pure () else error "source oracle failed"
  print (correct translated (12 :: Int),
    MT.toInteger (infcost translated fullDemand))
