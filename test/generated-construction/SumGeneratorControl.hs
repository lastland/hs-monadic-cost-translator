{-# LANGUAGE StandaloneDeriving, TypeFamilies #-}
module SumGeneratorControl where

import DatatypeGeneratorControl (sampleField)
import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as QC

data Producer = Empty | Filled (M.Th Int) (M.Th Bool)
data Observation = Absent | Present (M.ThD Int) (M.ThD Bool)

instance M.Demand Producer where
    type Dem Producer = Observation
    fullDemand Empty = Absent
    fullDemand (Filled left right) = Present (M.fullDemand left) (M.fullDemand right)
    usage Empty Absent = M.empty
    usage (Filled left right) (Present x y) = M.usage left x `M.union` (M.usage right y `M.union` M.empty)
    usage _ _ = M.empty

deriving instance Eq Observation
deriving instance Show Observation

instance QC.Arbitrary Observation where
    arbitrary = QC.sized (\size -> QC.oneof
        [pure Absent, Present <$> sampleField (size `div` 2) <*> sampleField (size `div` 2)])
    shrink _ = []
