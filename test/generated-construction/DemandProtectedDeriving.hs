{-# LANGUAGE StandaloneDeriving, TypeFamilies #-}
module DemandProtectedDeriving where

import qualified Memorist.Memorist as M

-- An actual Dem result still cannot grant methods on protected field types.
data Observation = Observation (M.Th Int)

instance M.Demand Observation where
    type Dem Observation = Observation
    fullDemand input = input

deriving instance Eq Observation
deriving instance Show Observation
