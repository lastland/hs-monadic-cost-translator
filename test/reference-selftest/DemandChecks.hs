module DemandChecks (main) where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import qualified DemandSupport as D
import qualified Memorist.Memorist as M

program :: () -> M.MemoristM Int
program _ = M.tick >> pure 5

codec :: D.DemandCodec Int
codec = D.toDemandCodec D.intPlan (const 0)

probe :: D.DemandProbe ()
probe = D.demandProbe "scalar" program (\_ value -> pure value) (\_ _ -> []) codec

main :: IO ()
main = do
    batch <- D.drawDemands 15 7 () probe
    replayed <- D.replayDemands () batch probe
    unless (replayed == [D.DemandObservation "scalar/budget-15/seed-7/original" (D.IntP 5) 1]) $
        fail "demand replay lost its actual value, requested plan, or tick count"
    rejected "wrong batch label" (D.replayDemands () (batch { D.batchLabel = "other" }) probe)
    rejected "negative draw budget" (D.drawDemands (-1) 7 () probe)
    rejected "negative replay budget" (D.replayDemands () (batch { D.batchBudget = -1 }) probe)
    rejected "missing typed decoder" (drawWith (codec { D.decodeDemand = const Nothing }))
    rejected "non-round-tripping decoder" (drawWith (codec { D.decodeDemand = const (Just 0) }))
    rejected "wrong observed constructor" (drawWith (codec { D.observeDemand = \_ _ -> D.ConP "wrong" [] }))
    rejected "excessive node budget" (drawWith (codec { D.budgetSize = const 17 }))
    rejected "negative node count" (drawWith (codec { D.budgetSize = const (-1) }))
    rejected "unchanged demand shrink" (D.drawDemands 15 7 ()
        (D.demandProbe "scalar" program (\_ value -> pure value) (\_ d -> [d]) codec))
    rejected "unbounded shrink batch" (D.replayDemands ()
        (batch { D.shrinkPlans = replicate 10001 (D.IntP 5) }) probe)
    rejected "infinite demand encoder" (drawWith
        (codec { D.encodeDemand = \_ -> D.ConP "infinite" (repeat D.SkipP) }))
    putStrLn "PASS: generated-demand codecs, budgets, compatibility, shrinking, and finite replay"
  where
    drawWith selected = D.drawDemands 15 7 ()
        (D.demandProbe "scalar" program (\_ value -> pure value) (\_ _ -> []) selected)

rejected :: String -> IO a -> IO ()
rejected label action = do
    outcome <- try (action >> pure ()) :: IO (Either SomeException ())
    case outcome of
        Left _ -> pure ()
        Right _ -> fail ("demand helper accepted " ++ label)
