module SumGeneratorEscape where

import SumGeneratorControl (Observation(..))
import qualified Test.QuickCheck as QC

ordinary :: [Observation]
ordinary = QC.oneof [pure Absent] `seq` []
