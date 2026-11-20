module OperandBoxControl where

import qualified Test.QuickCheck as Q

data Box = Box !Q.Property

prop_box :: Q.Property -> Q.Property
prop_box claim = let box = Box claim in case box of Box _ -> Q.property True

data Callback = Callback !(Bool -> Q.Property)

packCallback :: (Bool -> Q.Property) -> Callback
packCallback = Callback
