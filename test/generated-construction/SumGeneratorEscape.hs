module SumGeneratorEscape where

import SumGeneratorControl (Observation(..))
import qualified Test.QuickCheck as QC

ordinary :: QC.Gen Observation
ordinary = QC.oneof [pure Absent]
