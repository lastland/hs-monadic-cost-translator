module SizedDemandEscape where

import DemandControl (Observation(..))
import SizedDemandControl ()
import qualified Test.QuickCheck as QC

ordinary :: Char -> QC.Gen Char
ordinary input = QC.sized (`generate` input)
