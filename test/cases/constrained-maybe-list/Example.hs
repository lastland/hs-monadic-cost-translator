{-# LANGUAGE ScopedTypeVariables #-}
module Example where

expensive :: (Eq a, Num a) => a -> a
expensive n = if n == 0 then n + 1 else expensive (n - n) + 1

make :: (Eq a, Num a) => Int -> Maybe a
make n = if n == 0 then Nothing else Just (expensive 5)

one :: forall a. (Eq a, Num a) => a -> Int -> (Maybe [a], Maybe [a])
one _ _ =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
    in (shared :: Maybe [a], shared :: Maybe [a])

build :: (Eq a, Num a) => Int -> [a]
build n = if n <= 0 then [] else expensive 5 : build (n - 1)

views :: forall a. (Eq a, Num a) => a -> Int -> (Maybe [a], Maybe [a])
views _ n =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just (build n)
    in (shared :: Maybe [a], shared :: Maybe [a])

brokenHead :: forall a. (Eq a, Num a) => a -> (Maybe [a], Maybe [a])
brokenHead _ =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [error "Maybe-list head failure"]
    in (shared :: Maybe [a], shared :: Maybe [a])

brokenTail :: forall a. (Eq a, Num a) => a -> (Maybe [a], Maybe [a])
brokenTail _ =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just (expensive 5 : error "Maybe-list tail failure")
    in (shared :: Maybe [a], shared :: Maybe [a])
