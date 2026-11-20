module OperandWrappedProperty where

import OperandPropertyLaws
import qualified Test.QuickCheck as Q

draw :: Q.Gen Bool
draw = Q.arbitrary

prop_direct, prop_relay, prop_own, prop_scalar :: Q.Property
prop_direct = law draw
prop_relay = relay (Q.property True)
prop_own = own draw
prop_scalar = relay True
