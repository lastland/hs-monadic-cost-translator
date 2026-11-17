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
