module Main (main) where

import Control.Monad (forM_, unless)
import qualified Control.Monad.State.Class as State
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import qualified Memorist.Memorist as M
import qualified Memorist.Base.Strict as StrictRuntime

main :: IO ()
main = do
    ensure "ordinary lazy State" (12, 12) (Lazy.runState ordinaryLazy 7)
    ensure "ordinary strict State" ((7, 12), 12) (Strict.runState ordinaryStrict 7)
    forM_ [-13, -1, 0, 5, 7, 42] $ \left ->
        forM_ [-9, 0, 5, 7, 31] $ \right -> do
            observe "distinct computations" (2 * left + 2 * right, 2) $ do
                first <- M.lazy (work left)
                second <- M.lazy (work right)
                (+) <$> M.force first <*> M.force second
            observe "one retained thunk" (4 * left, 1) $ do
                thunk <- M.lazy (work left)
                (+) <$> M.force thunk <*> M.force thunk
            observe "repeated action" (4 * right, 2) ((+) <$> work right <*> work right)
    observe "unused bottom" (7, 0) (M.lazy (error "unused body" :: M.MemoristM Int) >> pure 7)
    let names = M.value (M.eval1 (do
            first <- M.lazy (work 7)
            second <- M.lazy (work 5)
            pure (M.tname first /= M.tname second)))
    ensure "distinct allocation names" True names
    let strictAction :: StrictRuntime.MemoristM Int
        strictAction = do
            first <- M.lazy (workStrict 7)
            second <- M.lazy (workStrict 5)
            (+) <$> M.force first <*> M.force second
        strictResult = M.eval1 strictAction
    ensure "strict runtime allocation" (24, 2)
        (M.value strictResult, M.toInteger (M.calcCost strictResult (M.fullDemand (M.value strictResult))))
    observe "shared work, direct" (20, 4) (withSharedCalls id)
    observe "shared work, uncached calls" (40, 7) (withSharedCalls (\call -> (+) <$> call <*> call))
    observe "shared work, cached call" (40, 4) (withSharedCalls (\call -> do
        result <- M.lazy call
        (+) <$> M.force result <*> M.force result))
    observe "shared work, separate evaluation" (20, 4) (withSharedCalls id)
    putStrLn "PASS: ordinary State and native allocation"

ordinaryLazy :: Lazy.State Int Int
ordinaryLazy = do
    value <- State.get
    State.put (value + 3)
    Lazy.modify (+ 2)
    Lazy.gets id

ordinaryStrict :: Strict.State Int (Int, Int)
ordinaryStrict = do
    value <- State.state (\old -> (old, old + 3))
    Strict.modify' (+ 2)
    current <- Strict.get
    pure (value, current)

work :: Int -> M.MemoristM Int
work value = M.tick >> pure (value + value)

workStrict :: Int -> StrictRuntime.MemoristM Int
workStrict value = M.tick >> pure (value + value)

-- Runtime preservation model only, not a proposed generated calling interface.
-- The retained work pays once; test, f, and g pay on every uncached call.
withSharedCalls :: (M.MemoristM Int -> M.MemoristM Int) -> M.MemoristM Int
withSharedCalls observeCalls = do
    shared <- M.lazy (work 5)
    let f = M.tick >> M.force shared
        g = M.tick >> M.force shared
        call = M.tick >> ((+) <$> f <*> g)
    observeCalls call

observe :: String -> (Int, Integer) -> M.MemoristM Int -> IO ()
observe label expected action = do
    let result = M.eval1 action
        value = M.value result
    ensure label expected (value, M.toInteger (M.calcCost result (M.fullDemand value)))

ensure :: (Eq a, Show a) => String -> a -> a -> IO ()
ensure label expected actual = unless (actual == expected) $
    fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
