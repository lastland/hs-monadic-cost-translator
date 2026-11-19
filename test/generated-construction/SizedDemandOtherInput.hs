{-# LANGUAGE TypeFamilies #-}
module SizedDemandOtherInput where

import DemandControl (Observation(..))
import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as QC

instance M.Demand Char where
    type Dem Char = Char
    fullDemand input = input

instance Observation Char where
    generate _ = pure

instance M.DemandArb Char where
    arbDemandFrom input = QC.sized (`generate` 'x')
