{-# LANGUAGE BangPatterns #-}
module OperandNamedBang where

import qualified Test.QuickCheck as Q

first :: a -> Bool
first x = let !y = x in True

atBool :: Bool -> Bool
atBool = first

prop_local :: Q.Gen Bool -> Q.Property
prop_local draw = let !y = draw in Q.property True

project :: (a, Bool) -> a
project (x, _) = let y = x in y

prop_project :: Q.Gen Bool -> Q.Property
prop_project draw = project (draw, False) `seq` Q.property True

prop_gen :: Q.Gen Bool -> Q.Property
prop_gen draw = Q.property (first draw)

atProperty :: Q.Property -> Bool
atProperty = first

forceComponent :: (a, Bool) -> Bool
forceComponent (x, _) = let !y = x in True

prop_callback :: (Bool -> Q.Property) -> Q.Property
prop_callback f = Q.property (forceComponent (f, False))
