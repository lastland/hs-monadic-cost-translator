{-# LANGUAGE ScopedTypeVariables #-}
module Example where

expensive :: (Eq a, Num a) => a -> a
expensive n = if n == 0 then n + 1 else expensive (n - n) + 1

one :: forall a. (Eq a, Num a) => a -> (a, a)
one input =
    let helper :: forall b. (Eq b, Num b) => b -> (b, b)
        helper _ =
            let shared :: forall c. (Eq c, Num c) => c
                shared = expensive 5
            in (shared :: b, shared :: b)
    in helper input

twice :: forall a. (Eq a, Num a) => a -> ((a, a), (a, a))
twice input =
    let helper :: forall b. (Eq b, Num b) => b -> (b, b)
        helper _ =
            let shared :: forall c. (Eq c, Num c) => c
                shared = expensive 5
            in (shared :: b, shared :: b)
    in (helper input, helper input)
