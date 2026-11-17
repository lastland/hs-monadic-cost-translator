{-# LANGUAGE ScopedTypeVariables #-}
module Example where

expensive :: (Eq a, Num a) => a -> a
expensive n = if n == 0 then n + 1 else expensive (n - n) + 1

one :: forall a. (Eq a, Num a) => a -> Int -> ((Maybe [a], Maybe [a]), (Maybe [a], Maybe [a]))
one input n =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper _ _ = (shared :: Maybe [b], shared :: Maybe [b])
    in (head (zipWith helper [input] [n]), input `helper` n)

many :: forall a. (Eq a, Num a) => [a] -> [Int] -> [(Maybe [a], Maybe [a])]
many inputs counts =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper _ _ = (shared :: Maybe [b], shared :: Maybe [b])
    in zipWith helper inputs counts
