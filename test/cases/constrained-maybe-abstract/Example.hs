{-# LANGUAGE ScopedTypeVariables #-}
module Example where

expensive :: (Eq a, Num a) => a -> a
expensive n = if n == 0 then n + 1 else expensive (n - n) + 1

make :: (Eq a, Num a) => Int -> Maybe a
make n = if n == 0 then Nothing else Just (expensive 5)

views :: forall a. (Eq a, Num a) => a -> Int -> (Maybe a, Maybe a)
views _ n =
    let shared :: forall b. (Eq b, Num b) => Maybe b
        shared = make n
    in (shared :: Maybe a, shared :: Maybe a)

broken :: forall a. (Eq a, Num a) => a -> (Maybe a, Maybe a)
broken _ =
    let shared :: forall b. (Eq b, Num b) => Maybe b
        shared = Just (error "abstract Maybe payload failure")
    in (shared :: Maybe a, shared :: Maybe a)
