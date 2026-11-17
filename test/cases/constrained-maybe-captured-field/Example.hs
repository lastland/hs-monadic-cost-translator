{-# LANGUAGE ScopedTypeVariables #-}
module Example where

expensive :: (Eq a, Num a) => a -> a
expensive n = if n == 0 then n + 1 else expensive (n - n) + 1

make :: (Eq a, Num a) => Int -> Maybe a
make n = if n == 0 then Nothing else Just (expensive 5)

one :: forall a b. (Eq a, Num a) => a -> b -> (Maybe (a, b), Maybe (a, b))
one _ fixed =
    let shared :: forall c. (Eq c, Num c) => Maybe (c, b)
        shared = Just (expensive 5, fixed)
    in (shared :: Maybe (a, b), shared :: Maybe (a, b))

brokenAbstract :: forall a b. (Eq a, Num a) => a -> b -> (Maybe (a, b), Maybe (a, b))
brokenAbstract _ fixed =
    let shared :: forall c. (Eq c, Num c) => Maybe (c, b)
        shared = Just (error "captured-field abstract failure", fixed)
    in (shared :: Maybe (a, b), shared :: Maybe (a, b))
