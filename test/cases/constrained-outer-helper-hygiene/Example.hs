{-# LANGUAGE ScopedTypeVariables #-}
module Example where

data Pair a b = Pair a b

expensive :: (Eq a, Num a) => a -> a
expensive n = if n == 0 then n + 1 else expensive (n - n) + 1

one :: forall a. (Eq a, Num a) => a -> (Maybe (Pair a a), Maybe (Pair a a))
one input =
    let helper :: forall b. (Eq b, Num b) => b -> (Maybe (Pair b b), Maybe (Pair b b))
        helper fixed =
            let shared :: forall a. (Eq a, Num a) => Maybe (Pair a b)
                shared = Just (Pair (expensive 5) fixed)
            in (shared :: Maybe (Pair b b), shared :: Maybe (Pair b b))
    in helper input
