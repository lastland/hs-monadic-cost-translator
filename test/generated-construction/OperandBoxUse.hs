module OperandBoxUse where

import OperandBoxControl (Box(..))
import qualified Test.QuickCheck as Q

pack :: Q.Property -> Box
pack =
    Box

prop_packed :: Q.Property -> Q.Property
prop_packed claim = case pack claim of Box _ -> Q.property True
