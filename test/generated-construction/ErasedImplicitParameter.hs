{-# LANGUAGE ImplicitParams, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module ErasedImplicitParameter where

import qualified Test.QuickCheck as Q

generic :: forall a. Eq a => a -> Q.Property
generic _ = Q.property True
generic input =
    let ?n = input == input
    in Q.property True

control :: (Eq a, Num a) => a -> Q.Property
control x = Q.property (x == 1)
control x =
    let ?n = (1 :: Int)
        ?box = Just x
    in Q.property (x == 1)
