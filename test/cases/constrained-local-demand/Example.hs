{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Example where

expensive :: (Eq a, Num a) => a -> a
expensive n
    | n == 0 = n + 1
    | otherwise = expensive (n - n) + 1

ignored :: Num a => a -> Bool
ignored _ = True

views :: forall t. t -> Int -> ((Int, Int), (Integer, Integer))
views _ n =
    let shared :: forall b. (Eq b, Num b) => b
        shared = if n < 0 then error "demanded local failure"
                 else (expensive 18446744073709551616 :: b)
    in ((shared, shared), (shared, shared))

inferred :: forall a. Num a => a -> ((Int, Int), (Integer, Integer))
inferred input =
    let shared = if ignored (input :: a)
                 then expensive 18446744073709551616 else expensive 0
    in ((shared, shared), (shared, shared))

distinct :: t -> ((Int, Int), (Integer, Integer))
distinct _ =
    let first :: (Eq b, Num b) => b
        first = expensive 18446744073709551616
        second :: (Eq b, Num b) => b
        second = expensive 18446744073709551616
    in ((first, second), (first, second))
