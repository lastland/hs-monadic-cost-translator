module CallerScalarEquality where

class Scalar a
instance Scalar Int
instance Scalar Integer
instance Scalar Bool
instance Scalar Float
instance Scalar Double
instance Scalar Char
instance Scalar ()

-- Scalar limits the operand universe. The caller separately promises that
-- demanding the result demands both operands and respects thunk accounting.
same :: (Scalar a, Eq a) => a -> a -> Bool
same x y = x == y
