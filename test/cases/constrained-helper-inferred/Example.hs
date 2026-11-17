{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Example where

expensive :: (Eq a, Num a) => a -> a
expensive n = if n == 0 then 0 else 1 + expensive (n - 1)

one :: forall a. (Eq a, Num a) => a -> (a, a)
one input =
    let helper value =
            let shared :: forall c. (Eq c, Num c) => c
                shared = expensive 5
            in (shared + value, shared + value)
    in helper input

twice :: forall a. (Eq a, Num a) => a -> ((a, a), (a, a))
twice input =
    let helper value =
            let shared :: forall c. (Eq c, Num c) => c
                shared = expensive 5
            in (shared + value, shared + value)
    in (helper input, helper input)
