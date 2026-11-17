{-# LANGUAGE ScopedTypeVariables #-}
module Example where

expensive :: (Eq a, Num a) => a -> a
expensive n = if n == 0 then n + 1 else expensive (n - n) + 1

one :: forall a. (Eq a, Num a) => a -> (a, a)
one _ =
    let shared :: forall b. (Eq b, Num b) => b
        shared = expensive 5
    in (shared :: a, shared :: a)
