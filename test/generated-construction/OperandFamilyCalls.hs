{-# LANGUAGE TypeFamilies #-}
module OperandFamilyCalls where

import OperandFamilies
import qualified Test.QuickCheck as Q

type instance Open Bool = Q.Property

draw :: Q.Gen Bool
draw = Q.arbitrary

prop_direct, prop_relay, prop_open, prop_scalar :: Q.Property
prop_direct = Q.property (h True draw)
prop_relay = law True draw
prop_open = Q.property (forceOpen True (Q.property True))
prop_scalar = law 'a' 'b'

-- This concrete family result is a property in this module.
local :: Open Bool
local = Q.property True

prop_generic, prop_strip :: Q.Property
prop_generic = generic True draw `seq` Q.property True
prop_strip = Q.property (strip [True] (Q.property True))
