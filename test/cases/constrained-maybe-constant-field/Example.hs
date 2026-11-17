{-# LANGUAGE ScopedTypeVariables #-}
module Example where

expensive :: (Eq a, Num a) => a -> a
expensive n = if n == 0 then n + 1 else expensive (n - n) + 1

make :: (Eq a, Num a) => Int -> Maybe a
make n = if n == 0 then Nothing else Just (expensive 5)

one :: forall a. (Eq a, Num a) => a -> Int -> (Maybe (a, Int), Maybe (a, Int))
one _ _ =
    let shared :: forall c. (Eq c, Num c) => Maybe (c, Int)
        shared = Just (expensive 5, 7)
    in (shared :: Maybe (a, Int), shared :: Maybe (a, Int))

brokenTuple :: forall a. (Eq a, Num a) => a -> (Maybe (a, Int), Maybe (a, Int))
brokenTuple _ =
    let shared :: forall c. (Eq c, Num c) => Maybe (c, Int)
        shared = Just (error "constant-field tuple failure")
    in (shared :: Maybe (a, Int), shared :: Maybe (a, Int))

brokenAbstract :: forall a. (Eq a, Num a) => a -> (Maybe (a, Int), Maybe (a, Int))
brokenAbstract _ =
    let shared :: forall c. (Eq c, Num c) => Maybe (c, Int)
        shared = Just (error "constant-field abstract failure", 7)
    in (shared :: Maybe (a, Int), shared :: Maybe (a, Int))

brokenFixed :: forall a. (Eq a, Num a) => a -> (Maybe (a, Int), Maybe (a, Int))
brokenFixed _ =
    let shared :: forall c. (Eq c, Num c) => Maybe (c, Int)
        shared = Just (expensive 5, error "constant-field fixed failure")
    in (shared :: Maybe (a, Int), shared :: Maybe (a, Int))

paid :: forall a. (Eq a, Num a) => a -> Int -> (Maybe (a, Int), Maybe (a, Int))
paid _ _ =
    let shared :: forall c. (Eq c, Num c) => Maybe (c, Int)
        shared = Just (expensive 5, expensive 5 :: Int)
    in (shared :: Maybe (a, Int), shared :: Maybe (a, Int))
