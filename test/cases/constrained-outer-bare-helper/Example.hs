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

twiceZip :: forall a. (Eq a, Num a) => [a] -> [Int] -> ([(Maybe [a], Maybe [a])], [(Maybe [a], Maybe [a])])
twiceZip inputs counts =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper _ _ = (shared :: Maybe [b], shared :: Maybe [b])
    in (zipWith helper inputs counts, zipWith helper inputs counts)

cachedZip :: forall a. (Eq a, Num a) => [a] -> [Int] -> ([(Maybe [a], Maybe [a])], [(Maybe [a], Maybe [a])])
cachedZip inputs counts =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper _ _ = (shared :: Maybe [b], shared :: Maybe [b])
        result = zipWith helper inputs counts
    in (result, result)

separateMany :: forall a. (Eq a, Num a) => [a] -> [Int] -> [(Maybe [a], Maybe [a])]
separateMany inputs counts =
    let leftShared :: forall c. (Eq c, Num c) => Maybe [c]
        leftShared = Just [expensive 5]
        rightShared :: forall c. (Eq c, Num c) => Maybe [c]
        rightShared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper _ _ = (leftShared :: Maybe [b], rightShared :: Maybe [b])
    in zipWith helper inputs counts

brokenHead :: forall a. (Eq a, Num a) => [a] -> [Int] -> [(Maybe [a], Maybe [a])]
brokenHead inputs counts =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [error "bare-helper head failure"]
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper _ _ = (shared :: Maybe [b], shared :: Maybe [b])
    in zipWith helper inputs counts

brokenRoot :: forall a. (Eq a, Num a) => [a] -> [Int] -> [(Maybe [a], Maybe [a])]
brokenRoot inputs counts =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = error "bare-helper root failure"
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper _ _ = (shared :: Maybe [b], shared :: Maybe [b])
    in zipWith helper inputs counts

wrapped :: forall a. (Eq a, Num a) => [a] -> [Int] -> [(Maybe [a], Maybe [a])]
wrapped inputs counts =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper _ _ = (shared :: Maybe [b], shared :: Maybe [b])
    in zipWith (helper) inputs counts
