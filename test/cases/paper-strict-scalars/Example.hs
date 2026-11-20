{-# LANGUAGE BangPatterns, FlexibleContexts #-}
-- shadowForward and shadowPlus test shadowing; givenAdd tests a closed
-- constraint at an expression whose caller supplies the standard dictionary.
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-simplifiable-class-constraints #-}
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

afterAnd, afterOr, nestedAnd :: Bool -> Int -> Int
afterAnd flag x = if flag && x > 0 then x + 1 else 0
afterOr flag x = if flag || x > 0 then 0 else x - 1
nestedAnd flag x = if flag && (x > 0 && x < 8) then x + 2 else 0

bangNext :: Int -> Int
bangNext x = let !y = work x in y + 1

letGuard, variableGuard :: Int -> Int
letGuard x
    | let y = x, x > 0 = y
    | otherwise = x - 1
variableGuard x
    | y <- x, x > 0 = y
    | otherwise = x - 1

checkedFlag :: Bool -> Bool
-- Keep both Bool uses visible to the translator.
{- HLINT ignore checkedFlag "Redundant if" -}
{- HLINT ignore checkedFlag "Redundant ==" -}
checkedFlag flag = if (flag :: Bool) then flag == True else False

aliasGuard :: Bool -> Int -> Maybe Int
aliasGuard enabled x
    | y <- x, enabled, x > 0 = Just y
    | otherwise = Just x

literal :: Int -> Int
literal x@0 = x + 1
literal y@1 = y + 2
literal _ = 0

inspect :: Int -> Int
inspect x = case x of
    0 -> x + 1
    _ -> x - 1

-- Both signed zeros match 0; the arithmetic must use the actual input.
zeroSign :: Double -> Int
zeroSign x@0 = if 1 / x < 0 then -1 else 1
zeroSign _ = 0

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
    -- Keep a source-function entry when forwarding the concrete scalar thunk.
    relay z = Just (z :: Int)

shadowPlus :: Int -> Int
shadowPlus x = x + error "right operand of local (+)"
  where
    (+) :: Int -> Int -> Int
    left + _ = left

givenAdd :: Int -> Maybe Int
givenAdd x = Just ((x + 1) :: Num Int => Int)

-- Both comparisons occupy the same logical span. Their values coexist in
-- the success branch and must still refer to different inputs.
mappedGuards :: Int -> Int -> Int
mappedGuards x y
{-# LINE 100 "Guard.hs" #-}
    | x > 0
{-# LINE 100 "Guard.hs" #-}
    , y > 0 = x - y
    | otherwise = 0

viaCase :: Int -> Int
viaCase x = case x of y -> y + x

nested :: Int -> Int
nested x = if x + 1 > 0 then x + 2 else x

caseRows :: Int -> Bool -> Int
caseRows x b = case b of
    _ | x > 0 -> 0
    _ -> x - 1

-- Nothing consumes the pattern-miss edge, leaving only guard failure.
coveredRows :: Int -> Maybe a -> Int
coveredRows x m = case m of
    Just _ | x > 0 -> 0
    Nothing -> 1
    _ -> x - 1

equation :: Int -> Int -> Int
equation x y | x > 0 = y
equation y x = y - x

-- Unlike the wildcard row, True can fail before the comparison runs.
joinRows :: Int -> Bool -> Maybe Int
joinRows x b = case b of
    True | x > 0 -> Nothing
    _ -> Just (x - 1)

-- A source constructor with an ordinary Int field and a generalized field
-- exercises the polymorphic payload emitter at two distinct instantiations.
data Packet a = Packet Int (Maybe a)

payload :: Int -> (Packet Bool, Packet Int)
payload x =
    let p :: Packet a
        p = if x > 0 then Packet (x + 1) Nothing else Packet 0 Nothing
    in (p, p)

payloadWork :: Int -> Int -> (Packet Bool, Packet Int)
payloadWork x y =
    let p :: Packet a
        p = if x > 0 then Packet (work (x + y)) Nothing else Packet 0 Nothing
    in (p, p)
