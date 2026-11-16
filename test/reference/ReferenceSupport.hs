{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | Cases supply computations, never properties, equality functions, or IO.
-- Constructors stay private: the observer owns forcing and cost measurement.
module ReferenceSupport
    ( Sample(..), Probe, scalar, scalarSource, boolean, integer, demanded, snapshot, failure, runProbes
    ) where

import Control.Exception (SomeAsyncException, SomeException, evaluate, fromException, throwIO, try)
import Control.Monad (unless)
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

runProbes :: [Probe] -> Sample -> IO [Result]
runProbes probes sample = do
    unless (any computation probes) (fail "case has no costed reference comparison")
    results <- mapM (`runProbe` sample) probes
    let labels = [label | Result label _ <- results]
    unless (length labels == length (unique labels)) (fail "duplicate observation labels")
    pure results
  where
    computation Computation{} = True
    computation Failure{} = True
    computation Snapshot{} = False
    unique [] = []
    unique (x:xs) = x : unique (filter (/= x) xs)

runProbe :: Probe -> Sample -> IO Result
runProbe (Computation label make project demand source) sample = do
    actual <- measured label $ do
        let result = M.eval1 (make sample)
            observed = show (project sample (M.value result))
            ticks = MT.toInteger (M.calcCost result (demand sample (M.value result)))
        _ <- evaluate (length observed)
        _ <- evaluate ticks
        pure (Returned observed (Just ticks))
    case source of
        Nothing -> pure actual
        Just oracle -> do
            expected <- measured label $ do
                let observed = show (oracle sample)
                _ <- evaluate (length observed)
                pure (Returned observed Nothing)
            unless (sameValue actual expected) (fail (label ++ ": source value differs: " ++ show (actual, expected)))
            pure actual
runProbe (Snapshot label make) sample = measured label $ do
    let observed = show (make sample)
    _ <- evaluate (length observed)
    pure (Returned observed Nothing)
runProbe (Failure label make) sample = do
    result <- runProbe (Computation label make (const id) (const M.fullDemand) Nothing) sample
    unless (result == Result label Raised) (fail (label ++ ": expected a demanded failure"))
    pure result

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
