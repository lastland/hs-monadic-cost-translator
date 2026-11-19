module NestedOrdinaryWhere where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

draw :: Q.Gen ()
draw = Q.elements [()]
  where
    deadShrink t = [M.tval t]

claim :: Q.Property
claim = Q.forAll draw (\x -> Q.property (x == ()))
