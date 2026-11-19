{-# LANGUAGE TypeApplications #-}
module NestedQuickCheckControl where

import qualified Memorist.Memorist as M
import StructuralViewControl (preserve)
import qualified Test.QuickCheck as Q

law :: M.Th Int -> Q.Property
law t = let observed = M.tval t in Q.property (observed == 3)

sampleLaw :: M.Th Int -> Q.Property
sampleLaw t = Q.forAll draw check
  where
    draw = let observed = M.tval t
               readings = pure @[] observed
           in Q.elements readings
    check n = Q.counterexample "sampled payload" (n == M.tval t)

copyDraw :: M.Th Int -> Q.Gen (M.Th Int)
copyDraw t = Q.elements [copy t]
  where
    copy v = preserve id v
