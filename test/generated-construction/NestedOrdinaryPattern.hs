module NestedOrdinaryPattern where

import qualified Test.QuickCheck as Q

class Choose a where
    choose :: a -> Bool
instance Choose Int where
    choose _ = True

draw :: Int -> Q.Gen ()
draw n = Q.elements [()]
  where
    (first, second) =
        (choose n, False)

law :: Int -> Q.Property
law n = let (first, second) = (choose n, False) in Q.property (first || second)
