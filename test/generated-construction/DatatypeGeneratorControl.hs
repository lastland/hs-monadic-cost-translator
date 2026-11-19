{-# LANGUAGE TypeFamilies #-}
module DatatypeGeneratorControl where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as QC

data Producer = Producer (M.Th Int)
data Observation = Observation (M.ThD Int)

instance M.Demand Producer where
    type Dem Producer = Observation
    fullDemand (Producer field) = Observation (M.fullDemand field)
    usage (Producer field) (Observation demanded) = M.usage field demanded `M.union` M.empty

sampleField :: QC.Arbitrary a => Int -> QC.Gen (M.ThD a)
sampleField size = if size <= 1 then pure M.Skip
    else QC.liftArbitrary (QC.resize (size - 2) QC.arbitrary)

instance QC.Arbitrary Observation where
    arbitrary = QC.sized (\size -> Observation <$> sampleField (size `div` 1))
    shrink _ = []
