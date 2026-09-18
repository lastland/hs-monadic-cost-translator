{-# LANGUAGE ScopedTypeVariables #-}
-- Standalone validation against the external MemoristANT package:
-- stack --stack-yaml integration/stack.yaml exec -- runghc -package=MemoristANT test/RuntimeScanlCandidate.hs
module Main (main) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (unless)
import System.Exit (exitFailure)
import System.Timeout (timeout)
import Memorist.Memorist
import Memorist.Data.List
import qualified Memorist.Base.Thunk as MT
import qualified Memorist.Example.List as ML
import qualified Memorist.Example.ListInf as MI

-- The proposed correction, independent of the external runtime implementation.
candidate :: Th (Th b -> Th a -> MemoristM b) -> Th b -> Th (ListT a)
          -> MemoristM (ListT b)
candidate tf tq tl = do
    tick
    tailThunk <- lazy $ do
        l <- force tl
        case l of
            NilT -> pure NilT
            ConsT tx txs -> do
                acc <- lazy $ do
                    f <- force tf
                    f tq tx
                candidate tf acc txs
    pure (ConsT tq tailThunk)

type Combiner = Th Int -> Th Int -> MemoristM Int
type Scan = Th Combiner -> Th Int -> Th (ListT Int) -> MemoristM (ListT Int)

addM :: Combiner
addM ta tb = do
    tick
    a <- force ta
    b <- force tb
    pure (a + b)

-- Same input and wrapper as RuntimeScanlGate.hs, parameterized only by scan.
badM :: Th Int -> MemoristM (ListT Int)
badM tx = do
    tick
    let condition = undefined :: Bool
    if condition then do
        tailThunk <- lazy (pure NilT)
        pure (ConsT tx tailThunk)
    else pure NilT

gate :: Scan -> Th Int -> MemoristM Int
gate scan tx = do
    tick
    callback <- lazy (pure addM)
    input <- lazy (badM tx)
    scanned <- lazy (scan callback tx input)
    ML.headM scanned

-- All other setup/observers are unticked. reprM creates zero-cost input thunks.
-- The callback factory has one tick, distinct from each addM invocation.
setup :: MemoristM Combiner -> MemoristM (ListT Int) -> MemoristM (ListT Int)
setup callback input = do
    tf <- lazy callback
    tq <- lazy (pure 10)
    tl <- lazy input
    MI.scanlM tf tq tl

finite :: [Int] -> MemoristM (ListT Int)
finite = reprM

secondThunk :: MemoristM (ListT Int) -> MemoristM (Th Int)
secondThunk scanned = do
    xs <- scanned
    case xs of
        ConsT _ tailThunk -> do
            rest <- force tailThunk
            case rest of
                ConsT acc _ -> pure acc
                NilT -> error "missing second accumulator"
        NilT -> error "missing seed"

-- Traverse every constructor, including terminal Nil, without forcing elements.
spineLength :: ListT Int -> MemoristM Int
spineLength NilT = pure 0
spineLength (ConsT _ ts) = do
    xs <- force ts
    n <- spineLength xs
    pure (n + 1)

-- Explicit finite demand: force exactly these output elements, skip final tail.
-- Int demand carries its value; scalar usage itself adds no thunk dependencies.
prefixDemand :: [Int] -> ListD Int
prefixDemand [] = error "prefixDemand requires a nonempty prefix"
prefixDemand [x] = ConsD (Need x) Skip
prefixDemand (x:xs) = ConsD (Need x) (Need (prefixDemand xs))

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual = do
    ok <- evaluate (actual == expected)
    unless ok $ fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
    putStrLn ("  " ++ label ++ " = " ++ show expected)

checkInt :: MemoristM Int -> Int -> Integer -> IO ()
checkInt computation expected ticks = do
    assertEqual "value" expected (value (eval1 computation))
    assertEqual "ticks" ticks (MT.toInteger (infcost computation fullDemand))

tests :: [(String, IO ())]
tests =
    [ ("external runtime: unused bottom input stays suspended", do
        checkInt (fromPure1 (gate MI.scanlM) (5 :: Int)) 5 3)
    , ("candidate: same bottom-input head gate", do
        -- Entry 1 + headM 1 + first scan call 1; badM is not demanded.
        checkInt (fromPure1 (gate candidate) (5 :: Int)) 5 (1 + 1 + 1))
    , ("spine only: bottom callback computation", do
        -- Three inputs produce four Cons cells: four scan calls, zero adds.
        -- This bottom also detects a misplaced monadic force in cost inference.
        let scanned = setup (error "callback computation forced")
                            (finite [undefined, undefined, undefined])
        checkInt (scanned >>= spineLength) 4 4)
    , ("spine only: bottom callback value", do
        let scanned = setup (pure (error "callback value forced"))
                            (finite [undefined, undefined, undefined])
        checkInt (scanned >>= spineLength) 4 4)
    , ("demand second accumulator; unused input tail is bottom", do
        -- Two scan calls + callback factory 1 + add 1. No third scan/input.
        let scanned = setup (tick >> pure addM) (finite (2 : undefined))
        checkInt (secondThunk scanned >>= force) 12 (2 + 1 + 1))
    , ("force the same accumulator twice: charge it once", do
        let scanned = setup (tick >> pure addM) (finite (2 : undefined))
        let twice = do
                acc <- secondThunk scanned
                a <- force acc
                b <- force acc
                pure (a + b)
        -- Same two scans, factory and add as above, despite two force calls.
        checkInt twice 24 (2 + 1 + 1))
    , ("finite demand on infinite ticked input", do
        let input = lazy (pure 1) >>= MI.enumFromM
        let scanned = setup (tick >> pure addM) input
        let expected = [10, 11, 13, 16] :: [Int]
        assertEqual "prefix values" expected
            (take 4 (abstract (value (eval1 scanned)) :: [Int]))
        -- Four scan calls + three input enum calls + factory 1 + three adds.
        assertEqual "ticks" (4 + 3 + 1 + 3 :: Integer)
            (MT.toInteger (infcost' scanned (prefixDemand expected))))
    , ("normal finite list, full values and terminal Nil", do
        let scanned = setup (tick >> pure addM) (finite [1, 2, 3])
        assertEqual "values" ([10, 11, 13, 16] :: [Int])
            (abstract (value (eval1 scanned)))
        -- n+1 scan calls, one shared factory, n adds; terminal Nil adds no tick.
        assertEqual "ticks" (4 + 1 + 3 :: Integer)
            (MT.toInteger (infcost scanned fullDemand)))
    , ("empty input, full demand with bottom callback", do
        let scanned = setup (error "empty callback forced") (finite [])
        assertEqual "values" ([10] :: [Int]) (abstract (value (eval1 scanned)))
        assertEqual "ticks" (1 :: Integer)
            (MT.toInteger (infcost scanned fullDemand)))
    ]

main :: IO ()
main = do
    outcomes <- mapM run tests
    unless (and outcomes) exitFailure
    putStrLn ("PASS: all " ++ show (length tests) ++ " checks")
  where
    run (label, action) = do
        putStrLn label
        result <- try (timeout (10 * 1000000) action)
        case result of
            Right (Just ()) -> pure True
            Right Nothing -> putStrLn "  FAIL: timed out after 10 seconds" >> pure False
            Left (e :: SomeException) ->
                putStrLn ("  FAIL: " ++ displayException e) >> pure False
