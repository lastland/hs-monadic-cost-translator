{-# LANGUAGE FlexibleContexts #-}
module DemandDerivingEscape where

import DemandDerivingControl (Observation)
import qualified Memorist.Memorist as M

ordinary :: Observation -> Bool
ordinary input = input == input

given :: Eq (M.ThD Int) => M.ThD Int -> Bool
given input = input == input
