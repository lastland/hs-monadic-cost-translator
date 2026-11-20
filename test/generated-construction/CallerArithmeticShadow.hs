module CallerArithmeticShadow where

import Prelude hiding (Num, (+))

class Num a where
    (+) :: a -> a -> a

twice :: Num a => a -> a
twice x = x + x
