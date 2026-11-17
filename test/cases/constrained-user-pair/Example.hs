{-# LANGUAGE ScopedTypeVariables #-}
module Example where

expensive :: (Eq a, Num a) => a -> a
expensive n = if n == 0 then n + 1 else expensive (n - n) + 1

make :: (Eq a, Num a) => Int -> Maybe a
make n = if n == 0 then Nothing else Just (expensive 5)

data Pair a b = Pair a b

one :: forall a b. (Eq a, Num a) => a -> b -> (Maybe (Pair a b), Maybe (Pair a b))
one _ fixed =
    let shared :: forall c. (Eq c, Num c) => Maybe (Pair c b)
        shared = Just (Pair (expensive 5) fixed)
    in (shared :: Maybe (Pair a b), shared :: Maybe (Pair a b))

brokenAbstract :: forall a b. (Eq a, Num a) => a -> b -> (Maybe (Pair a b), Maybe (Pair a b))
brokenAbstract _ fixed =
    let shared :: forall c. (Eq c, Num c) => Maybe (Pair c b)
        shared = Just (Pair (error "user-Pair abstract failure") fixed)
    in (shared :: Maybe (Pair a b), shared :: Maybe (Pair a b))

brokenPair :: forall a b. (Eq a, Num a) => a -> b -> (Maybe (Pair a b), Maybe (Pair a b))
brokenPair _ _ =
    let shared :: forall c. (Eq c, Num c) => Maybe (Pair c b)
        shared = Just (error "user-Pair root failure")
    in (shared :: Maybe (Pair a b), shared :: Maybe (Pair a b))

twins :: forall a. (Eq a, Num a) => a -> (Maybe (Pair a a), Maybe (Pair a a))
twins _ =
    let shared :: forall c. (Eq c, Num c) => Maybe (Pair c c)
        shared = Just (Pair (expensive 5) (expensive 5))
    in (shared :: Maybe (Pair a a), shared :: Maybe (Pair a a))
