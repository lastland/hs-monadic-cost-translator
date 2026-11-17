{-# LANGUAGE ScopedTypeVariables #-}
module Example where

expensive :: (Eq a, Num a) => a -> a
expensive n = if n == 0 then n + 1 else expensive (n - n) + 1

make :: (Eq a, Num a) => Int -> Maybe a
make n = if n == 0 then Nothing else Just (expensive 5)

one :: forall a. (Eq a, Num a) => a -> Int -> (Maybe (Maybe a), Maybe (Maybe a))
one _ n =
    let shared :: forall c. (Eq c, Num c) => Maybe (Maybe c)
        shared = Just (make n)
    in (shared :: Maybe (Maybe a), shared :: Maybe (Maybe a))

brokenInner :: forall a. (Eq a, Num a) => a -> (Maybe (Maybe a), Maybe (Maybe a))
brokenInner _ =
    let shared :: forall c. (Eq c, Num c) => Maybe (Maybe c)
        shared = Just (error "nested Maybe inner failure")
    in (shared :: Maybe (Maybe a), shared :: Maybe (Maybe a))

brokenLeaf :: forall a. (Eq a, Num a) => a -> (Maybe (Maybe a), Maybe (Maybe a))
brokenLeaf _ =
    let shared :: forall c. (Eq c, Num c) => Maybe (Maybe c)
        shared = Just (Just (error "nested Maybe leaf failure"))
    in (shared :: Maybe (Maybe a), shared :: Maybe (Maybe a))
