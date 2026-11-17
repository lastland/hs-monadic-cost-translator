{-# LANGUAGE ScopedTypeVariables #-}
module Example where

expensive :: (Eq a, Num a) => a -> a
expensive n = if n == 0 then n + 1 else expensive (n - n) + 1

make :: (Eq a, Num a) => Int -> Maybe a
make n = if n == 0 then Nothing else Just (expensive 5)

views :: forall p. p -> Int -> (Maybe Int, Maybe Int)
views _ n =
    let shared :: forall a. (Eq a, Num a) => Maybe a
        shared = make n
    in (shared :: Maybe Int, shared :: Maybe Int)

broken :: p -> (Maybe Int, Maybe Int)
broken _ =
    let shared :: forall a. (Eq a, Num a) => Maybe a
        shared = Just (error "local Maybe payload failure")
    in (shared :: Maybe Int, shared :: Maybe Int)
