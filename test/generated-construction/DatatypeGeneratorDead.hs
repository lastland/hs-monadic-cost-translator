{-# LANGUAGE TypeFamilies #-}
module DatatypeGeneratorDead where

import DatatypeGeneratorControl (sampleField)
import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as QC

data Producer = Producer (M.Th Int)
data Observation = Observation (M.ThD Int)

instance M.Demand Producer where
    type Dem Producer = Observation
    fullDemand (Producer field) = Observation (M.fullDemand field)
    usage (Producer field) (Observation demanded) = M.usage field demanded `M.union` M.empty

instance QC.Arbitrary Observation where
    arbitrary = QC.sized (\size ->
        let unused = M.value (M.eval1 (pure 17 :: M.MemoristM Int))
        in Observation <$> sampleField (size `div` 1))
    shrink _ = []
