{-# LANGUAGE FlexibleContexts, RebindableSyntax, TypeFamilies #-}
module ReboundDemand where

import Prelude
import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as QC

ifThenElse :: Bool -> a -> a -> a
ifThenElse condition yes no = case condition of
    True -> yes
    False -> no

class M.Demand a => Observation a where
    generate :: Int -> a -> QC.Gen (M.Dem a)
    shrinkCompatible :: a -> M.Dem a -> [M.Dem a]

generateField :: Observation a => Int -> M.Th a -> QC.Gen (M.ThD (M.Dem a))
generateField size input = if size <= 1 then pure M.Skip
    else QC.liftArbitrary (generate (size - 2) (M.tval input))
