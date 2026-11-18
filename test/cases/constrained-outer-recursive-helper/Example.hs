{-# LANGUAGE ScopedTypeVariables #-}
module Example where

expensive :: (Eq a, Num a) => a -> a
expensive n = if n == 0 then n + 1 else expensive (n - n) + 1

one :: forall a. (Eq a, Num a) => a -> Int -> ((Maybe [a], Maybe [a]), (Maybe [a], Maybe [a]))
one input n =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper x count = if count == 0 then (shared :: Maybe [b], shared :: Maybe [b]) else helper x (count - 1)
    in (head (zipWith helper [input] [n]), input `helper` n)

oneLoop :: forall a. (Eq a, Num a) => a -> Int -> (Maybe [a], Maybe [a])
oneLoop input n =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper x k = if k == 0 then (shared :: Maybe [b], shared :: Maybe [b]) else helper x (k - 1)
    in helper input n

twice :: forall a. (Eq a, Num a) => a -> Int -> ((Maybe [a], Maybe [a]), (Maybe [a], Maybe [a]))
twice input n =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper x k = if k == 0 then (shared :: Maybe [b], shared :: Maybe [b]) else helper x (k - 1)
    in (helper input n, helper input n)

cached :: forall a. (Eq a, Num a) => a -> Int -> ((Maybe [a], Maybe [a]), (Maybe [a], Maybe [a]))
cached input n =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper x k = if k == 0 then (shared :: Maybe [b], shared :: Maybe [b]) else helper x (k - 1)
    in let result = helper input n in (result, result)

frames :: forall a. (Eq a, Num a) => a -> Int -> [(Maybe [a], Maybe [a])]
frames input n =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> Int -> [(Maybe [b], Maybe [b])]
        helper x k = if k == 0 then [(shared :: Maybe [b], shared :: Maybe [b])] else (shared :: Maybe [b], shared :: Maybe [b]) : helper x (k - 1)
    in helper input n

separate :: forall a. (Eq a, Num a) => a -> Int -> ((Maybe [a], Maybe [a]), (Maybe [a], Maybe [a]))
separate input n =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [expensive 5]
        other :: forall c. (Eq c, Num c) => Maybe [c]
        other = Just [expensive 5]
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper x k = if k == 0 then (shared :: Maybe [b], other :: Maybe [b]) else helper x (k - 1)
    in (helper input n, helper input n)

brokenHead :: forall a. (Eq a, Num a) => a -> Int -> ((Maybe [a], Maybe [a]), (Maybe [a], Maybe [a]))
brokenHead input n =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = Just [error "recursive head failure"]
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper x k = if k == 0 then (shared :: Maybe [b], shared :: Maybe [b]) else helper x (k - 1)
    in (helper input n, helper input n)

brokenRoot :: forall a. (Eq a, Num a) => a -> Int -> ((Maybe [a], Maybe [a]), (Maybe [a], Maybe [a]))
brokenRoot input n =
    let shared :: forall c. (Eq c, Num c) => Maybe [c]
        shared = error "recursive root failure"
        helper :: forall b. (Eq b, Num b) => b -> Int -> (Maybe [b], Maybe [b])
        helper x k = if k == 0 then (shared :: Maybe [b], shared :: Maybe [b]) else helper x (k - 1)
    in (helper input n, helper input n)
