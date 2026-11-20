{-# LANGUAGE CPP #-}
module OriginCPP where

#include "ORIGIN_INCLUDE"

data Box = Box Int

keep :: Int -> Int
keep x = IDENTITY(x)

partial :: [Int] -> [Int]
partial xs = map keep xs
