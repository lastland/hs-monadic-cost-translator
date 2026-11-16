{-# OPTIONS_GHC -Wall -Werror #-}
module Compare (main) where

import Control.Exception (bracket)
import Control.Monad (forM, unless)
import Data.List (intercalate)
import Protocol
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (BufferMode(LineBuffering), Handle, hFlush, hGetLine, hPrint, hSetBuffering)
import System.Process (CreateProcess(..), ProcessHandle, StdStream(..), createProcess, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Test.QuickCheck (Args(..), Gen, arbitrary, chooseInt, counterexample, forAllShrink, ioProperty, isSuccess, quickCheckWithResult, shrink, stdArgs, vectorOf)
import Test.QuickCheck.Random (mkQCGen)
import Text.Read (readMaybe)

type Server = (Handle, Handle, ProcessHandle)

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        [generated, reference] ->
            bracket (start generated) stop $ \actual ->
            bracket (start reference) stop $ \expected -> do
                checkDemandBudgets actual expected
                mapM_ (compareSample actual expected) boundaries
                mapM_ (check actual expected) [20261114, 20261115]
        _ -> fail "usage: compare GENERATED_OBSERVER REFERENCE_OBSERVER"

start :: FilePath -> IO Server
start executable = do
    (Just input, Just output, _, process) <- createProcess (proc executable [])
        { std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }
    hSetBuffering input LineBuffering
    pure (input, output, process)

stop :: Server -> IO ()
stop (_, _, process) = do
    terminateProcess process
    _ <- waitForProcess process
    pure ()

check :: Server -> Server -> Int -> IO ()
check actual expected seed = do
    result <- quickCheckWithResult
        (stdArgs { maxSuccess = 100, maxSize = 12, replay = Just (mkQCGen seed, 0) }) $
        forAllShrink samples shrinkSample $ \sample -> ioProperty $ do
            (generated, reference) <- observeSample actual expected seed sample
            pure $ counterexample ("seed: " ++ show seed ++ "\n" ++ differences generated reference)
                (generated == reference)
    unless (isSuccess result) exitFailure

compareSample :: Server -> Server -> Sample -> IO ()
compareSample actual expected sample = do
    (generated, reference) <- observeSample actual expected 20261114 sample
    unless (generated == reference) $
        fail ("boundary sample: " ++ show sample ++ "\n" ++ differences generated reference)

-- At least 200 budget-15 draws, explicit zero budgets, and every budget through
-- 25. The reference never draws: it replays the actual plans and complete shrink
-- lists received from the generated worker.
checkDemandBudgets :: Server -> Server -> IO ()
checkDemandBudgets actual expected = case boundaries of
    [] -> fail "comparison boundary schedule is empty"
    sample:_ -> do
        initial <- draw actual 0 20261114 sample
        unless (null initial) $ mapM_ (checkBudget sample) [0..25]
  where
    checkBudget sample budget = do
        (_, generated, reference) <- observeAt actual expected budget (20261114 + budget) sample
        unless (generated == reference) $
            fail ("demand budget: " ++ show budget ++ "\n" ++ differences [generated] [reference])

observeSample :: Server -> Server -> Int -> Sample -> IO ([Response], [Response])
observeSample actual expected seed sample = do
    (hasPlans, generated, reference) <- observeAt actual expected 15 seed sample
    extra <- if hasPlans
        then forM [0, (abs (number sample) + 3 * variant sample) `mod` 26] $ \budget -> do
            (_, a, b) <- observeAt actual expected budget (seed + 31 * budget) sample
            pure (a,b)
        else pure []
    pure (generated : map fst extra, reference : map snd extra)

observeAt :: Server -> Server -> Int -> Int -> Sample -> IO (Bool, Response, Response)
observeAt actual expected budget seed sample = do
    batches <- draw actual budget seed sample
    generated <- evaluateServer actual sample batches
    reference <- evaluateServer expected sample batches
    pure (not (null batches), generated, reference)

draw :: Server -> Int -> Int -> Sample -> IO [DemandBatch]
draw server budget seed sample = do
    response <- request server (Draw budget seed sample)
    case response of
        Drawn batches -> do
            unless (all ((== budget) . batchBudget) batches &&
                    map batchSeed batches == take (length batches) [seed..]) $
                fail "worker changed the controller's demand budget or seed"
            pure batches
        _ -> fail "worker did not return generated demand plans"

evaluateServer :: Server -> Sample -> [DemandBatch] -> IO Response
evaluateServer server sample batches = do
    response <- request server (Evaluate sample batches)
    case response of
        Evaluated{} -> pure response
        _ -> fail "worker did not return observations"

boundaries :: [Sample]
boundaries =
    [Sample n m xs w r l d v
    | (n,m,xs,w,r,v) <- [(5,0,[1,2,3],3,4,0), (0,0,[],0,1,0), (-1,1,[-1,0,1],1,8,1)]
    , (l,d) <- [(False,False),(True,False),(False,True),(True,True)]]

request :: Server -> Request -> IO Response
request (input, output, _) command = do
    hPrint input command
    hFlush input
    response <- timeout 30000000 (hGetLine output)
    case response >>= readMaybe of
        Just responseValue -> pure responseValue
        Nothing -> fail "observer timed out or returned an invalid response"

samples :: Gen Sample
samples = Sample <$> chooseInt (-40, 40) <*> chooseInt (-40, 40)
    <*> (chooseInt (0, 6) >>= \n -> vectorOf n (chooseInt (-20, 20)))
    <*> chooseInt (0, 5) <*> chooseInt (1, 8) <*> arbitrary <*> arbitrary <*> chooseInt (0, 7)

shrinkSample :: Sample -> [Sample]
shrinkSample s =
    [s { number = n } | n <- shrink (number s)] ++
    [s { other = n } | n <- shrink (other s)] ++
    [s { numbers = ns } | ns <- shrink (numbers s)] ++
    [s { width = n } | n <- shrink (width s), n >= 0] ++
    [s { repeats = n } | n <- shrink (repeats s), n >= 1] ++
    [s { leftDemand = False } | leftDemand s] ++
    [s { rightDemand = False } | rightDemand s] ++
    [s { variant = n } | n <- shrink (variant s), n >= 0]

differences :: [Response] -> [Response] -> String
differences actual expected = intercalate "\n" (take 20 messages)
  where
    messages = ["different response counts" | length actual /= length expected] ++
        concat [responseDifferences a b | (a,b) <- zip actual expected]
    responseDifferences (Evaluated a da) (Evaluated b db) =
        ["different observation counts" | length a /= length b || length da /= length db] ++
        ["generated: " ++ show x ++ "\nreference: " ++ show y | (x,y) <- zip a b, x /= y] ++
        ["generated demand: " ++ show x ++ "\nreference demand: " ++ show y | (x,y) <- zip da db, x /= y]
    responseDifferences a b = ["unexpected responses: " ++ show (a,b) | a /= b]
