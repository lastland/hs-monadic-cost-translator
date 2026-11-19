module DatatypeGeneratorEscape where

import DatatypeGeneratorControl (Observation(..), sampleField)
import qualified Test.QuickCheck as QC

ordinary :: QC.Gen Observation
ordinary = QC.sized (\size -> Observation <$> sampleField (size `div` 1))
