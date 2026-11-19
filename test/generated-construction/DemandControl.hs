{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module DemandControl where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as QC

-- Deliberately different names: the declaration role is its complete schema.
class M.Demand a => Observation a where
    generate :: Int -> a -> QC.Gen (M.Dem a)
    shrinkCompatible :: a -> M.Dem a -> [M.Dem a]
    shrinkCompatible _ _ = []

generateField :: Observation a => Int -> M.Th a -> QC.Gen (M.ThD (M.Dem a))
generateField size input =
    if size <= 1 then pure M.Skip
    else QC.liftArbitrary (generate (size - 2) (M.tval input))

shrinkView :: Observation a => M.Th a -> M.ThD (M.Dem a) -> [M.ThD (M.Dem a)]
shrinkView input = QC.liftShrink (shrinkCompatible (M.tval input))

instance Observation Int where
    generate _ = pure
