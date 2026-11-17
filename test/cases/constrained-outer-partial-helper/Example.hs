{-# LANGUAGE ScopedTypeVariables #-}
module Example where

expensive :: (Eq a, Num a) => a -> a
expensive n = if n == 0 then n + 1 else expensive (n - n) + 1

one :: forall a. (Eq a, Num a) => a -> [(Maybe [a], Maybe [a])]
one input =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper _ _ = (shared :: Maybe [b], shared :: Maybe [b])
    in map (helper input) [0]

many :: forall a. (Eq a, Num a) => a -> [Int] -> [(Maybe [a], Maybe [a])]
many input counts =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper _ _ = (shared :: Maybe [b], shared :: Maybe [b])
    in map (helper input) counts

twice :: forall a. (Eq a, Num a) => a -> [Int] -> ([(Maybe [a], Maybe [a])], [(Maybe [a], Maybe [a])])
twice input counts =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper _ _ = (shared :: Maybe [b], shared :: Maybe [b])
    in (map (helper input) counts, map (helper input) counts)

cachedMap :: forall a. (Eq a, Num a) => a -> [Int] -> ([(Maybe [a], Maybe [a])], [(Maybe [a], Maybe [a])])
cachedMap input counts =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper _ _ = (shared :: Maybe [b], shared :: Maybe [b])
        result = map (helper input) counts
    in (result, result)

separateMany :: forall a. (Eq a, Num a) => a -> [Int] -> [(Maybe [a], Maybe [a])]
separateMany input counts =
    let leftShared :: forall c. (Eq c, Num c) => Maybe [c]
        leftShared = Just [expensive 5]
        rightShared :: forall c. (Eq c, Num c) => Maybe [c]
        rightShared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper _ _ = (leftShared :: Maybe [b], rightShared :: Maybe [b])
    in map (helper input) counts

mixed :: forall a. (Eq a, Num a) => a -> [Int] -> ((Maybe [a], Maybe [a]), [(Maybe [a], Maybe [a])])
mixed input counts =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper _ _ = (shared :: Maybe [b], shared :: Maybe [b])
    in (helper input 0, map (helper input) counts)

brokenHead :: forall a. (Eq a, Num a) => a -> [Int] -> [(Maybe [a], Maybe [a])]
brokenHead input counts =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [error "partial-helper head failure"]
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper _ _ = (shared :: Maybe [b], shared :: Maybe [b])
    in map (helper input) counts

brokenRoot :: forall a. (Eq a, Num a) => a -> [Int] -> [(Maybe [a], Maybe [a])]
brokenRoot input counts =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = error "partial-helper root failure"
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper _ _ = (shared :: Maybe [b], shared :: Maybe [b])
    in map (helper input) counts
