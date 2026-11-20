module Main where

import Control.Monad (unless)
import qualified Generated as G
import qualified Reference as R
import qualified Memorist.Memorist as M
import qualified Memorist.Base.Thunk as MT

-- Each arithmetic method evaluates its operands. No instance method observes a
-- thunk, evaluates a Memorist action, or reads/writes runtime accounting.
data StrictNumber = StrictNumber !Int deriving (Eq, Show)

instance Num StrictNumber where
    StrictNumber x + StrictNumber y = StrictNumber (x + y)
    StrictNumber x - StrictNumber y = StrictNumber (x - y)
    StrictNumber x * StrictNumber y = StrictNumber (x * y)
    negate (StrictNumber x) = StrictNumber (negate x)
    abs (StrictNumber x) = StrictNumber (abs x)
    signum (StrictNumber x) = StrictNumber (signum x)
    fromInteger x = StrictNumber (fromInteger x)

unNumber :: StrictNumber -> Int
unNumber (StrictNumber x) = x

observe :: (a -> Int) -> M.MemoristM a -> (Int, Integer)
observe project computation =
    let result = M.eval1 (project <$> computation)
        value = M.value result
    in (value, MT.toInteger (M.calcCost result (M.fullDemand value)))

run :: a -> (M.Th a -> M.MemoristM a) -> M.MemoristM a
run value twice = M.lazy (M.tick >> pure value) >>= twice

main :: IO ()
main = do
    let inputs = [-100 .. 100]
        check n =
            let expected = (n + n, 2)
            in observe id (run n G.twiceM) == expected &&
                observe id (run n R.twiceM) == expected &&
                observe unNumber (run (StrictNumber n) G.twiceM) == expected &&
                observe unNumber (run (StrictNumber n) R.twiceM) == expected
    unless (all check inputs) (fail "caller arithmetic values or ticks disagree")
    putStrLn "PASS: full generated twice, Int and strict custom Num, 201 inputs, value=2*n and ticks=2, independent Reference"
