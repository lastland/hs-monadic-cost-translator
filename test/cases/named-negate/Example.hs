module Example where

import Prelude hiding (negate)
import qualified Prelude as P

named :: Int -> Int
named x = P.negate x

large :: Integer -> Integer
large x = P.negate x

negate :: Int -> Int
negate x = x + 1

shadowed :: Int -> Int
shadowed x = negate x

repeated :: Int -> Int
repeated x = P.negate x + P.negate x

mapped :: [Int] -> [Int]
mapped xs = map P.negate xs

double :: Int -> Int
double x = x + x

composed :: Int -> Int
composed x = (P.negate . double) x

keep :: a -> b -> a
keep x _ = x

ignored :: Int -> Int
ignored x = keep x (P.negate (error "unused negation operand" :: Int))
