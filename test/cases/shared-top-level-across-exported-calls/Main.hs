module Main where
import Generated
import qualified Example as Source
import Memorist.Memorist
import qualified Memorist.Base.Thunk as MT
translated :: MemoristM Int
translated = do { bindings <- initialize; a <- fromPure1 (firstM bindings) (5::Int); b <- fromPure1 (secondM bindings) (5::Int); pure (a+b) }
main = do
  if (Source.first (5::Int) + Source.second 5) == (30 :: Int) then pure () else error "source oracle failed"
  print (correct translated (30 :: Int),
    MT.toInteger (infcost translated fullDemand))
