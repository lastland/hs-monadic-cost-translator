{-# LANGUAGE FlexibleContexts #-}
module CallerArithmeticOperations where

add, subtractNumber, multiply :: Num a => a -> a -> a
add x y = x + y
subtractNumber x y = x - y
multiply x y = x * y

negative :: Num a => a -> a
negative x = -x

integerLiteral :: Num a => a
integerLiteral = 1

fractionalLiteral :: Fractional a => a
fractionalLiteral = 1.5

divide :: Fractional a => a -> a -> a
divide x y = x / y

divideInteger, modulo, quotient, remainder :: Integral a => a -> a -> a
divideInteger x y = div x y
modulo x y = mod x y
quotient x y = quot x y
remainder x y = rem x y

-- Integral selects Num through its actual Real superclass; this is not
-- permission for Real's Ord/Eq methods.
superclassAdd :: Integral a => a -> a
superclassAdd x = x + 1

class Num a => Arithmetic a

homeSuperclass :: Arithmetic a => a -> a
homeSuperclass x = x + 1
