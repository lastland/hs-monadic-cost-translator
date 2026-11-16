{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Cases supply computations, never properties, equality functions, or IO.
-- Constructors stay private: the observer owns forcing and cost measurement.
module ReferenceSupport
    ( Sample(..), Probe, scalar, scalarSource, boolean, integer, demanded, snapshot, failure
    , generatedDemands, drawProbeDemands, runProbes
    ) where

import Control.Exception (SomeAsyncException, SomeException, evaluate, fromException, throwIO, try)
import Control.Monad (unless, when)
import qualified DemandSupport as D
import qualified Memorist.Memorist as M
import qualified Memorist.Base.Thunk as MT
import Protocol
import System.Timeout (timeout)

data Probe where
    Computation :: (M.Demand a, Show b) =>
        String -> (Sample -> M.MemoristM a) -> (Sample -> a -> b) ->
        (Sample -> a -> M.Dem a) -> Maybe (Sample -> b) -> Probe
    Snapshot :: Show a => String -> (Sample -> a) -> Probe
    Failure :: String -> (Sample -> M.MemoristM Int) -> Probe
    Generated :: D.DemandProbe Sample -> Probe

scalar :: String -> (Sample -> M.MemoristM Int) -> Probe
scalar label = demanded label (const id) (const M.fullDemand)

scalarSource :: String -> (Sample -> Int) -> (Sample -> M.MemoristM Int) -> Probe
scalarSource label source program = Computation label program (const id) (const M.fullDemand) (Just source)

boolean :: String -> (Sample -> M.MemoristM Bool) -> Probe
boolean label = demanded label (const id) (const M.fullDemand)

integer :: String -> (Sample -> M.MemoristM Integer) -> Probe
integer label = demanded label (const id) (const M.fullDemand)

demanded :: (M.Demand a, Show b) =>
    String -> (Sample -> a -> b) -> (Sample -> a -> M.Dem a) ->
    (Sample -> M.MemoristM a) -> Probe
demanded label project demand program = Computation label program project demand Nothing

-- Structural checks supplement, but cannot replace, costed computations.
snapshot :: Show a => String -> (Sample -> a) -> Probe
snapshot = Snapshot

failure :: String -> (Sample -> M.MemoristM Int) -> Probe
failure = Failure

generatedDemands :: D.DemandProbe Sample -> Probe
generatedDemands = Generated

validateCatalog :: [Probe] -> IO ()
validateCatalog probes = do
    unless (any computation probes) (fail "case has no costed reference comparison")
    let labels = map probeLabel probes
    when (any null labels) (fail "empty observation label")
    unless (length labels == length (unique labels)) (fail "duplicate observation labels")
  where
    computation Computation{} = True
    computation Failure{} = False
    computation Snapshot{} = False
    computation Generated{} = True
    unique [] = []
    unique (x:xs) = x : unique (filter (/= x) xs)

probeLabel :: Probe -> String
probeLabel (Computation label _ _ _ _) = label
probeLabel (Snapshot label _) = label
probeLabel (Failure label _) = label
probeLabel (Generated probe) = D.demandLabel probe

drawProbeDemands :: [Probe] -> Int -> Int -> Sample -> IO [DemandBatch]
drawProbeDemands probes budget seed sample = do
    validateCatalog probes
    sequence [D.drawDemands budget (seed + index) sample probe
        | (index, probe) <- zip [0..] [p | Generated p <- probes]]

runProbes :: [Probe] -> Sample -> [DemandBatch] -> IO ([Result], [DemandObservation])
runProbes probes sample batches = do
    validateCatalog probes
    let planned = [probe | Generated probe <- probes]
    unless (map D.demandLabel planned == map batchLabel batches) $
        fail "missing, reordered, duplicated, or undeclared generated-demand batch"
    ordinary <- sequence [runProbe probe sample | probe <- probes, not (isGenerated probe)]
    replayed <- sequence [D.replayDemands sample batch probe | (probe, batch) <- zip planned batches]
    pure (ordinary, concat replayed)
  where
    isGenerated Generated{} = True
    isGenerated _ = False

runProbe :: Probe -> Sample -> IO Result
runProbe (Computation label make project demand source) sample = do
    actual <- measureComputation label (make sample) (project sample) (demand sample)
    case source of
        Nothing -> do
            unless (actual /= Result label Raised) (fail (label ++ ": unexpected computation failure"))
            pure actual
        Just oracle -> do
            expected <- measured label $ do
                let observed = show (oracle sample)
                _ <- evaluate (length observed)
                pure (Returned observed Nothing)
            unless (sameValue actual expected) (fail (label ++ ": source value differs: " ++ show (actual, expected)))
            pure actual
runProbe (Snapshot label make) sample = do
    result <- measured label $ do
        let observed = show (make sample)
        _ <- evaluate (length observed)
        pure (Returned observed Nothing)
    unless (result /= Result label Raised) (fail (label ++ ": structural observation failed"))
    pure result
runProbe (Failure label make) sample = do
    result <- measureComputation label (make sample) id M.fullDemand
    unless (result == Result label Raised) (fail (label ++ ": expected a demanded failure"))
    pure result
runProbe Generated{} _ = fail "internal error: generated demand was not replayed"

measureComputation :: (M.Demand a, Show b) =>
    String -> M.MemoristM a -> (a -> b) -> (a -> M.Dem a) -> IO Result
measureComputation label program project demand = measured label $ do
    let result = M.eval1 program
        observed = show (project (M.value result))
        ticks = MT.toInteger (M.calcCost result (demand (M.value result)))
    _ <- evaluate (length observed)
    _ <- evaluate ticks
    pure (Returned observed (Just ticks))

sameValue :: Result -> Result -> Bool
sameValue (Result _ (Returned actual _)) (Result _ (Returned expected _)) = actual == expected
sameValue (Result _ Raised) (Result _ Raised) = True
sameValue _ _ = False

measured :: String -> IO Outcome -> IO Result
measured label action = do
    -- Catch synchronous failures *outside* timeout. Two timeouts are not evidence
    -- of equivalent programs, and must never be compared as ordinary failures.
    result <- try (timeout 5000000 action) :: IO (Either SomeException (Maybe Outcome))
    case result of
        Right (Just outcome) -> pure (Result label outcome)
        Right Nothing -> fail (label ++ ": observation timed out")
        Left exception -> case fromException exception :: Maybe SomeAsyncException of
            Just _ -> throwIO exception
            Nothing -> pure (Result label Raised)
