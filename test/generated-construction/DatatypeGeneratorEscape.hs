module DatatypeGeneratorEscape where

import DatatypeGeneratorControl (Observation(..), sampleField)
import qualified Test.QuickCheck as QC

ordinary :: [Observation]
ordinary = QC.sized (\size -> Observation <$> sampleField (size `div` 1)) `seq` []
