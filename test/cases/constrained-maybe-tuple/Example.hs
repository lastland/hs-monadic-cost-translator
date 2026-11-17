{-# LANGUAGE ScopedTypeVariables #-}
module Example where

expensive :: (Eq a, Num a) => a -> a
expensive n = if n == 0 then n + 1 else expensive (n - n) + 1

make :: (Eq a, Num a) => Int -> Maybe a
make n = if n == 0 then Nothing else Just (expensive 5)

one :: forall a. (Eq a, Num a) => a -> Int -> (Maybe (a, a), Maybe (a, a))
one _ _ =
    let shared :: forall c. (Eq c, Num c) => Maybe (c, c)
        shared = Just (expensive 5, expensive 5)
    in (shared :: Maybe (a, a), shared :: Maybe (a, a))

brokenTuple :: forall a. (Eq a, Num a) => a -> (Maybe (a, a), Maybe (a, a))
brokenTuple _ =
    let shared :: forall c. (Eq c, Num c) => Maybe (c, c)
        shared = Just (error "Maybe-tuple tuple failure")
    in (shared :: Maybe (a, a), shared :: Maybe (a, a))

brokenLeft :: forall a. (Eq a, Num a) => a -> (Maybe (a, a), Maybe (a, a))
brokenLeft _ =
    let shared :: forall c. (Eq c, Num c) => Maybe (c, c)
        shared = Just (error "Maybe-tuple left failure", expensive 5)
    in (shared :: Maybe (a, a), shared :: Maybe (a, a))

brokenRight :: forall a. (Eq a, Num a) => a -> (Maybe (a, a), Maybe (a, a))
brokenRight _ =
    let shared :: forall c. (Eq c, Num c) => Maybe (c, c)
        shared = Just (expensive 5, error "Maybe-tuple right failure")
    in (shared :: Maybe (a, a), shared :: Maybe (a, a))
