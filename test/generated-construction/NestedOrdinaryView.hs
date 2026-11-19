module NestedOrdinaryView where

import qualified Memorist.Memorist as M
import StructuralViewControl (preserve)
import qualified Test.QuickCheck as Q

draw :: Bool -> M.Th Int -> Q.Gen (M.Th Int)
draw flag t = Q.elements [change t, t]
  where
    {-# NOINLINE change #-}
    change v = preserve (\n -> if flag then negate n else n) v

control :: M.Th Int -> Q.Gen (M.Th Int)
control t = Q.elements [copy t]
  where
    copy v = preserve id v
