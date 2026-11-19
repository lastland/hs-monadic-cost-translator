{-# LANGUAGE TypeFamilies #-}
module SizedDemandDead where

import DemandControl (Observation(..))
import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as QC

instance M.Demand Char where
    type Dem Char = Char
    fullDemand input = input

instance Observation Char where
    generate _ = pure
    shrinkCompatible _ _ = let unused = M.value (M.eval1 (pure 17 :: M.MemoristM Int)) in []

instance M.DemandArb Char where
    arbDemandFrom input = QC.sized (`generate` input)
