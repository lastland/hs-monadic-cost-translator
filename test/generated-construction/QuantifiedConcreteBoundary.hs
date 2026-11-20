{-# LANGUAGE TypeFamilies #-}
module QuantifiedConcreteBoundary where
import UtilityApp
import qualified Memorist.Memorist as M

-- Utility superclass validation does not trace a concrete DemandF closure.
data Concrete = Concrete (HigherT M.MaybeT Int)
instance M.Demand Concrete where
    type Dem Concrete = HigherD M.MaybeD Int
    fullDemand (Concrete x) = M.fullDemand x
    usage (Concrete x) dx = M.usage x dx
