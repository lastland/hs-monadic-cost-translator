{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Example where

producer :: Int -> [a]
producer n = if n <= 0 then [] else undefined : producer (n - 1)

spine :: [a] -> Int
spine [] = 0
spine (_:xs) = 1 + spine xs

ignore :: a -> Int
ignore _ = 1

scoped :: forall b. b -> Int -> Int
scoped input n =
    let shared :: forall b. [b]
        shared = (producer n :: [b])
    in ignore (input :: b) + spine (shared :: [Int]) + spine (shared :: [Bool])

views :: forall b. b -> Int -> ([Int], [Bool])
views _ n =
    let shared :: forall b. [b]
        shared = (producer n :: [b])
    in (shared, shared)

inferred :: forall a. a -> ([Int], [Bool])
inferred input =
    let shared = if ignore (input :: a) == 1 then [] else producer 1
    in (shared, shared)

inferredSiblings :: forall a a'. a -> a' -> ([a], ([Int], [Bool]))
inferredSiblings input sibling =
    let kept = [input :: a]
        first = if ignore (input :: a) == 1 then [] else producer 1
        second = if ignore (sibling :: a') == 1 then first else producer 1
    in (kept, (first, second))
