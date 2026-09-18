{-# LANGUAGE ScopedTypeVariables #-}
-- Keep the source control-flow shape explicit: this probe tests its suspension.
{- HLINT ignore "Use list comprehension" -}
-- Independent feasibility gate: run in the external-runtime integration environment.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Memorist.Memorist
import Memorist.Data.List
import qualified Memorist.Example.List as ML
import qualified Memorist.Example.ListInf as MI
import qualified Memorist.Base.Thunk as MT

source :: Int -> Int
source x = head (scanl add x (bad x))
  where
    add a b = a + b
    bad n = if (undefined :: Bool) then [n] else []

addM :: Th Int -> Th Int -> MemoristM Int
addM ta tb = do
    tick
    a <- force ta
    b <- force tb
    pure (a + b)

badM :: Th Int -> MemoristM (ListT Int)
badM tx = do
    tick
    let condition = undefined :: Bool
    if condition then do
        tailThunk <- lazy (pure NilT)
        pure (ConsT tx tailThunk)
    else pure NilT

handwritten :: Th Int -> MemoristM Int
handwritten tx = do
    tick
    callback <- lazy (pure addM)
    input <- lazy (badM tx)
    scanned <- lazy (MI.scanlM callback tx input)
    ML.headM scanned

main :: IO ()
main = do
    print ("source", source 5)
    outcome <- try (evaluate (correct (fromPure1 handwritten (5 :: Int)) (5 :: Int)))
    case outcome of
        Left (e :: SomeException) -> fail ("handwritten runtime scanl violates source laziness: " ++ show e)
        Right ok -> if ok then print ("handwritten agrees", ok) else fail "value disagreement"
    cost <- try (evaluate (MT.toInteger (infcost (fromPure1 handwritten (5 :: Int)) fullDemand)))
    case cost of
        Left (e :: SomeException) -> fail ("scanl cost observation forces the unused input: " ++ show e)
        Right n -> if n == 3 then print ("cost", n) else fail ("expected entry + head + scanl = 3 ticks, got " ++ show n)
