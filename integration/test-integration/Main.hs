module Main (main) where

import Control.Monad ( unless )
import Data.Either ( isLeft )
import qualified Test.QuickCheck as QC
import Test.Tasty
import Test.Tasty.HUnit

import Probes

main :: IO ()
main = defaultMain tests

-- | Run a QuickCheck property as a tasty test case (fails on falsity,
-- surfacing the counterexample).
qc :: QC.Testable prop => String -> prop -> TestTree
qc name prop = testCase name $ do
    r <- QC.quickCheckResult prop
    unless (QC.isSuccess r) (assertFailure (QC.output r))

costCase :: String -> Integer -> Integer -> TestTree
costCase name computed expected =
    testCase name (computed @?= expected)

tests :: TestTree
tests = testGroup "cost-translate integration"
    [ testGroup "P1 named callback (map succ)"
        [ costCase "full demand 2n+2 (n=3)" (p1Cost [1, 2, 3]) (p1Expected [1, 2, 3])
        , costCase "full demand 2n+2 (n=0)" (p1Cost []) (p1Expected [])
        , costCase "partial demand 2-of-3: 2k+1" p1PartialCost p1PartialExpected
        , qc "exact cost 2n+2 on random lists" $ \xs ->
              p1Cost xs QC.=== p1Expected (xs :: [Int])
        , qc "value correctness" $ \xs -> p1Correct (xs :: [Int])
        ]
    , testGroup "P2 partial application at 3-ary function"
        [ qc "sumWith (foldr callback, 1 of 3): 2k+2" $ \(n :: Int) xs ->
              p2SumWithCost n xs QC.=== p2SumWithExpected xs
        , qc "sumWith value correctness" $ \(n :: Int) xs ->
              p2SumWithCorrect n (xs :: [Int])
        , qc "mapAdd (map callback, 2 of 3): 2k+2" $ \(n :: Int) (m :: Int) xs ->
              p2MapAddCost n m xs QC.=== p2MapAddExpected xs
        , qc "mapAdd value correctness" $ \(n :: Int) (m :: Int) xs ->
              p2MapAddCorrect n m (xs :: [Int])
        ]
    , testGroup "P3 local function capturing a variable"
        [ qc "example: 2k+2" $ \(n :: Int) xs ->
              p3ExampleCost n xs QC.=== p3ExampleExpected xs
        , qc "example value correctness" $ \(n :: Int) xs ->
              p3ExampleCorrect n (xs :: [Int])
        , qc "shared expensive local n: 2k+6" $ \xs ->
              p3Example3Cost xs QC.=== p3Example3Expected (xs :: [Int])
        , qc "example3 value correctness" $ \xs ->
              p3Example3Correct (xs :: [Int])
        , qc "non-shared contrast: 6k+2" $ \xs ->
              p3Example3NSCost xs QC.=== p3Example3NSExpected (xs :: [Int])
        ]
    , testGroup "P4 composition chains"
        [ qc "2-chain: 3k+2" $ \xs ->
              p4MapSucc2Cost xs QC.=== p4MapSucc2Expected (xs :: [Int])
        , qc "2-chain value correctness" $ \xs ->
              p4MapSucc2Correct (xs :: [Int])
        , qc "2-chain parity with ML.mapSuccM2" $ \xs ->
              p4ParityMapSuccM2 (xs :: [Int])
        , qc "3-chain: 4k+2" $ \xs ->
              p4MapSucc3Cost xs QC.=== p4MapSucc3Expected (xs :: [Int])
        , qc "3-chain value correctness" $ \xs ->
              p4MapSucc3Correct (xs :: [Int])
        ]
    , testGroup "P5 shared top-level value"
        [ costCase "spine-expensive shared: 4" (p5AnswerCost 100) p5AnswerExpected
        , costCase "spine-expensive non-shared: 5" (p5AnswerNSCost 100) p5AnswerNSExpected
        , testCase "spine-expensive value correctness" (p5AnswerCorrect 100 @?= True)
        , costCase "element-expensive shared: 7" (p5Answer2Cost 100) p5Answer2Expected
        , costCase "element-expensive non-shared: 11" (p5Answer2NSCost 100) p5Answer2NSExpected
        , testCase "element-expensive value correctness" (p5Answer2Correct 100 @?= True)
        ]
    , testGroup "P6 guard-order sensitivity"
        [ testCase "takeA 0 on bottom spine: demanded evaluation fails" $ do
              r <- p6TakeAOnBottom
              assertBool "expected an exception from the forced bottom" (isLeft r)
        , testCase "takeB 0 on bottom spine: cost is 1" $ do
              r <- p6TakeBCostOnBottom
              either (assertFailure . ("unexpected exception: " ++) . show) (@?= 1) r
        , testCase "takeB 0 on bottom spine: value is []" $ do
              r <- p6TakeBValueOnBottom
              either (assertFailure . ("unexpected exception: " ++) . show) (@?= []) r
        , testCase "defined input: both variants cost n+1" $ do
              p6TakeAOnDefined 2 [1 .. 5] @?= 3
              p6TakeBOnDefined 2 [1 .. 5] @?= 3
              p6TakeAOnDefined 0 [1 .. 5] @?= 1
              p6TakeBOnDefined 0 [1 .. 5] @?= 1
        , qc "A/B cost parity on random inputs" $ \(n :: Int) xs ->
              p6ParityAB n (xs :: [Int])
        , qc "guard-first parity with ML.takeM" $ \(n :: Int) xs ->
              p6ParityBML n (xs :: [Int])
        , qc "value correctness (both variants)" $ \(n :: Int) xs ->
              p6CorrectA n xs QC..&&. p6CorrectB n (xs :: [Int])
        , costCase "partial demand avoids bottom tail" p6PartialDemandCost p6PartialDemandExpected
        ]
    , testGroup "P7 foldr with named append vs concatM"
        [ costCase "[[1,2],[3]]: L+2K+2 = 9"
            (p7Cost [[1, 2], [3]]) (p7Expected [[1, 2], [3]])
        , qc "exact cost L+2K+2 on random inputs" $ \xss ->
              p7Cost xss QC.=== p7Expected (xss :: [[Int]])
        , qc "value correctness" $ \xss -> p7Correct (xss :: [[Int]])
        , qc "cost parity with ML.concatM" $ \xss ->
              p7ParityConcatM (xss :: [[Int]])
        ]
    ]
