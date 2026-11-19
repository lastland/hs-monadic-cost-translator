{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module AlteredDemandView where

import DemandControl (Observation(..))
import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as QC

-- A genuine compiler-accepted helper, not the checked two-argument schema.
generateField :: Observation a => Int -> M.Th a -> M.Th a -> QC.Gen (M.ThD (M.Dem a))
generateField size input other =
    if size <= 1 then pure M.Skip
    else QC.liftArbitrary (generate (size - 2) (M.tval other))
