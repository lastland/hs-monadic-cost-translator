module Compare (main) where

import Control.Exception (bracket)
import Control.Monad (unless)
import Data.List (intercalate)
import Protocol
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (BufferMode(LineBuffering), Handle, hFlush, hGetLine, hPutStrLn, hSetBuffering)
import System.Process (CreateProcess(..), ProcessHandle, StdStream(..), createProcess, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Test.QuickCheck (Args(..), Gen, arbitrary, chooseInt, counterexample, forAllShrink, ioProperty, isSuccess, quickCheckWithResult, shrink, stdArgs, vectorOf, (===))
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
            generated <- observe actual sample
            reference <- observe expected sample
            pure $ counterexample ("seed: " ++ show seed ++ "\n" ++ differences generated reference)
                (generated === reference)
    unless (isSuccess result) exitFailure

compareSample :: Server -> Server -> Sample -> IO ()
compareSample actual expected sample = do
    generated <- observe actual sample
    reference <- observe expected sample
    unless (generated == reference) $
        fail ("boundary sample: " ++ show sample ++ "\n" ++ differences generated reference)

boundaries :: [Sample]
boundaries =
    [Sample n m xs w r l d v
    | (n,m,xs,w,r,v) <- [(5,0,[1,2,3],3,4,0), (0,0,[],0,1,0), (-1,1,[-1,0,1],1,8,1)]
    , (l,d) <- [(False,False),(True,False),(False,True),(True,True)]]

observe :: Server -> Sample -> IO [Result]
observe (input, output, _) sample = do
    hPutStrLn input (show sample)
    hFlush input
    response <- timeout 30000000 (hGetLine output)
    case response >>= readMaybe of
        Just results -> pure results
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

differences :: [Result] -> [Result] -> String
differences actual expected = intercalate "\n"
    ["generated: " ++ show a ++ "\nreference: " ++ show b | (a,b) <- zip actual expected, a /= b]
