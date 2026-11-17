{-# LANGUAGE ScopedTypeVariables #-}
module Example where

expensive :: (Eq a, Num a) => a -> a
expensive n = if n == 0 then n + 1 else expensive (n - n) + 1

make :: (Eq a, Num a) => Int -> Maybe a
make n = if n == 0 then Nothing else Just (expensive 5)

one :: forall a. (Eq a, Num a) => a -> Int -> (Maybe [a], Maybe [a])
one input _ =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> (Maybe [b], Maybe [b])
        helper _ = (shared :: Maybe [b], shared :: Maybe [b])
    in helper input

twice :: forall a. (Eq a, Num a) => a -> ((Maybe [a], Maybe [a]), (Maybe [a], Maybe [a]))
twice input =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> (Maybe [b], Maybe [b])
        helper _ = (shared :: Maybe [b], shared :: Maybe [b])
    in (helper input, helper input)

cachedHelper :: forall a. (Eq a, Num a) => a -> ((Maybe [a], Maybe [a]), (Maybe [a], Maybe [a]))
cachedHelper input =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> (Maybe [b], Maybe [b])
        helper _ = (shared :: Maybe [b], shared :: Maybe [b])
        result = helper input
    in (result, result)

many :: forall a. (Eq a, Num a) => a -> Int -> [(Maybe [a], Maybe [a])]
many input count =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> (Maybe [b], Maybe [b])
        helper _ = (shared :: Maybe [b], shared :: Maybe [b])
        loop :: Int -> [(Maybe [a], Maybe [a])]
        loop n = if n <= 0 then [] else helper input : loop (n - 1)
    in loop count

separate :: forall a. (Eq a, Num a) => a -> ((Maybe [a], Maybe [a]), (Maybe [a], Maybe [a]))
separate input =
    let leftShared :: forall c. (Eq c, Num c) => Maybe [c]
        leftShared = Just [expensive 5]
        rightShared :: forall c. (Eq c, Num c) => Maybe [c]
        rightShared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> (Maybe [b], Maybe [b])
        helper _ = (leftShared :: Maybe [b], rightShared :: Maybe [b])
    in (helper input, helper input)

brokenHead :: forall a. (Eq a, Num a) => a -> ((Maybe [a], Maybe [a]), (Maybe [a], Maybe [a]))
brokenHead input =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [error "outer-helper head failure"]
        helper :: forall b. (Eq b, Num b) => b -> (Maybe [b], Maybe [b])
        helper _ = (shared :: Maybe [b], shared :: Maybe [b])
    in (helper input, helper input)

brokenRoot :: forall a. (Eq a, Num a) => a -> ((Maybe [a], Maybe [a]), (Maybe [a], Maybe [a]))
brokenRoot input =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = error "outer-helper root failure"
        helper :: forall b. (Eq b, Num b) => b -> (Maybe [b], Maybe [b])
        helper _ = (shared :: Maybe [b], shared :: Maybe [b])
    in (helper input, helper input)
