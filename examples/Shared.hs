module Shared where

seed :: Int
seed = double 5

double x = x + x

first x = seed + x

second x = seed + x

expensive n | n == 0 = n + 1
            | otherwise = expensive (n - n) + 1

shared :: (Eq a, Num a) => a
shared = expensive 18446744073709551616

asInt :: Int -> Int
asInt x = shared + x

asInteger :: Integer -> Integer
asInteger x = shared + x
