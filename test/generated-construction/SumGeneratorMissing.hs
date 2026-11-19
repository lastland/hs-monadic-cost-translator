{-# LANGUAGE TypeFamilies #-}
module SumGeneratorMissing where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as QC

data Observation = Absent | Present (M.ThD Int)

instance M.Demand Observation where
    type Dem Observation = Observation
    fullDemand input = input

-- The six-combinator contract does not prescribe a generator's alternatives.
instance QC.Arbitrary Observation where
    arbitrary = QC.sized (\size -> QC.oneof [pure Absent, pure Absent])
    shrink _ = []
