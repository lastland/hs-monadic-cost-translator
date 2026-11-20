module Example where

import Prelude hiding (take)

take :: Int -> [a] -> [a]
take n xs =
    if 0 < n then
        case xs of
            []       -> []
            (y : ys) -> y : take (n - 1) ys
    else []

work :: Int -> Int
work z = z + z

boxNext :: Int -> Maybe Int
boxNext x = if x > 0 then Just (work (x + 1)) else Nothing

boxPrefix :: Int -> Maybe Int
boxPrefix x = if (>) x 0 then Just (work ((+) x 1)) else Nothing

boxQuot :: Bool -> Int -> Maybe Int
boxQuot use x = if x >= 0 then Just (choose (12 `div` x)) else Nothing
  where
    choose :: Int -> Int
    choose q = if use then q else 7

-- The fallback can be reached both before and after testing x.
guarded :: Bool -> Int -> Maybe Int
guarded check x
    | check, x <= 0 = Nothing
    | otherwise = Just (work (x + 1))

suspended :: Int -> Int -> (Int, Int)
suspended x y =
    (if x > 0 then let add z = x + z in add y else 0, 3 + work (x - 1))

shadowForward :: Int -> Maybe Int -> Maybe Int
shadowForward x input
    | x > 0 = case input of
        Nothing -> Nothing
        Just x -> if x < 0 then relay x else Nothing
    | otherwise = relay x
  where
    relay z = Just z

-- Both comparisons occupy the same logical span. Their values coexist in
-- the success branch and must still refer to different inputs.
mappedGuards :: Int -> Int -> Int
mappedGuards x y
{-# LINE 100 "Guard.hs" #-}
    | x > 0
{-# LINE 100 "Guard.hs" #-}
    , y > 0 = x - y
    | otherwise = 0
