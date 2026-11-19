{-# LANGUAGE TypeFamilies #-}
module DemandManualInstance where

import qualified Memorist.Memorist as M

data Producer = Producer (M.Th Int)
data Observation = Observation (M.ThD Int)

instance M.Demand Producer where
    type Dem Producer = Observation
    fullDemand (Producer field) = Observation (M.fullDemand field)
    usage (Producer field) (Observation demanded) = M.usage field demanded `M.union` M.empty

instance Eq Observation where
    Observation left == Observation right = left == right
