{-# LANGUAGE TypeFamilies #-}
module SumGeneratorDead where

import DatatypeGeneratorControl (sampleField)
import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as QC

data Observation = Absent | Present (M.ThD Int)

instance M.Demand Observation where
    type Dem Observation = Observation
    fullDemand input = input

instance QC.Arbitrary Observation where
    arbitrary = QC.sized (\size -> QC.oneof
        [pure Absent, Present <$> sampleField (size `div` 1)])
    shrink _ = let unused = M.value (M.eval1 (pure 17 :: M.MemoristM Int)) in []
