{-# LANGUAGE TypeFamilies #-}
module StandaloneDemandShrink where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as QC

class M.Demand a => GeneratedDemand a where
    genDemandBudget :: Int -> a -> QC.Gen (M.Dem a)
    shrinkDemandCompatible :: a -> M.Dem a -> [M.Dem a]
    shrinkDemandCompatible _ _ = []

instance GeneratedDemand Int where
    genDemandBudget _ = pure
    shrinkDemandCompatible value demanded = [value | demanded > value]

shrinkField :: GeneratedDemand a => M.Th a -> M.ThD (M.Dem a) -> [M.ThD (M.Dem a)]
shrinkField value = QC.liftShrink (shrinkDemandCompatible (M.tval value))

data BoxT = BoxT (M.Th Int)
data BoxD = BoxD (M.ThD Int)

instance M.Demand BoxT where
    type Dem BoxT = BoxD
    fullDemand (BoxT value) = BoxD (M.fullDemand value)
    usage (BoxT value) (BoxD demanded) = M.usage value demanded

instance GeneratedDemand BoxT where
    genDemandBudget _ _ = pure (BoxD M.Skip)
    shrinkDemandCompatible (BoxT value) (BoxD demanded) =
        [BoxD smaller | smaller <- shrinkField value demanded]

shrinkBox :: BoxT -> BoxD -> [BoxD]
shrinkBox = shrinkDemandCompatible

standalone :: BoxT -> BoxD -> [BoxD]
standalone value demanded = shrinkBox value demanded
