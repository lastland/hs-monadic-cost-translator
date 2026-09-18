{-# LANGUAGE ScopedTypeVariables #-}
-- Run in the integration Stack environment. This is a feasibility probe,
-- deliberately separate from the tests of actual generated output in Main.hs.
-- The partial 'head' is the probe's subject: it validates the headM
-- replacement's laziness, so rewriting it away would defeat the probe.
{-# OPTIONS_GHC -Wno-x-partial #-}
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Memorist.Memorist
import Memorist.Data.List
import qualified Memorist.Example.List as ML

source :: Int -> Int
source x = head (filter keep (x : error "source tail bottom"))
  where keep _ = True

keepM :: Th a -> MemoristM Bool
keepM _ = tick >> pure True

handwritten :: Th Int -> MemoristM Int
handwritten tx = do
    tick
    predicate <- lazy (pure keepM)
    tailThunk <- lazy (pure (error "source tail bottom"))
    input <- lazy (pure (ConsT tx tailThunk))
    filtered <- lazy (ML.filterM predicate input)
    ML.headM filtered

main :: IO ()
main = do
    print ("source", source 5)
    outcome <- try (evaluate (correct (fromPure1 handwritten (5 :: Int)) (5 :: Int)))
    case outcome of
        Left (e :: SomeException) -> fail ("handwritten runtime filter violates source laziness: " ++ show e)
        Right ok -> print ("handwritten agrees", ok)
