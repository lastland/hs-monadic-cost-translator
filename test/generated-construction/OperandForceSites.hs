{-# LANGUAGE BangPatterns #-}
module OperandForceSites where

import Prelude hiding (seq)
import qualified Prelude as P
import qualified Test.QuickCheck as Q

first :: (a, b) -> a
first (x, _) = x

lazyFirst :: (a, b) -> a
lazyFirst ~(x, _) = x

after :: b -> a -> a
after trigger x = trigger `P.seq` x

seq :: a -> b -> b
seq _ fallback = fallback

strictId :: a -> a
strictId x = x `P.seq` x

bangFirst :: (a, b) -> a
bangFirst (!x, _) = x

prop_first, prop_lazy, prop_second, prop_rebound :: Q.Gen Bool -> Q.Property
prop_first draw = first (draw, False) `P.seq` Q.property True
prop_lazy draw = lazyFirst (draw, False) `P.seq` Q.property True
prop_second draw = after True draw `P.seq` Q.property True
prop_rebound draw = (draw `seq` draw) `P.seq` Q.property True

ordinary :: (Q.Gen Bool, Bool) -> Bool
ordinary pair = first pair `P.seq` True

prop_strict, prop_bang :: Q.Gen Bool -> Q.Property
prop_strict draw = strictId draw `P.seq` Q.property True
prop_bang draw = bangFirst (draw, False) `P.seq` Q.property True
