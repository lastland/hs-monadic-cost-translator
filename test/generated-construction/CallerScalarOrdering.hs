module CallerScalarOrdering where

class Scalar a
instance Scalar Int
instance Scalar Integer
instance Scalar Bool
instance Scalar Float
instance Scalar Double
instance Scalar Char
instance Scalar ()

different :: (Scalar a, Eq a) => a -> a -> Bool
different x y = x /= y

less :: (Scalar a, Ord a) => a -> a -> Bool
less x y = x < y

lessEqual :: (Scalar a, Ord a) => a -> a -> Bool
lessEqual x y = x <= y

greater :: (Scalar a, Ord a) => a -> a -> Bool
greater x y = x > y

greaterEqual :: (Scalar a, Ord a) => a -> a -> Bool
greaterEqual x y = x >= y

-- Eq evidence is selected through the actual Ord superclass.
equalThroughOrd :: (Scalar a, Ord a) => a -> a -> Bool
equalThroughOrd x y = x == y

-- This is a supplied dictionary slot even at Int, not a preselected standard
-- instance. The checker records the caller obligation; it does not prove it.
suppliedInt :: Eq Int => Int -> Int -> Bool
suppliedInt x y = x == y
