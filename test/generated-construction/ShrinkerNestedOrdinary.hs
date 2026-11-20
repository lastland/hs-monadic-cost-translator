{-# LANGUAGE TypeFamilies #-}
module ShrinkerNestedOrdinary where

import StandaloneDemandShrink (GeneratedDemand(..))
import qualified Memorist.Memorist as M

data Target = Target (M.Th Int)

instance M.Demand Target where
    type Dem Target = Int
    fullDemand (Target value) = M.tval value
    usage _ _ = M.empty

instance GeneratedDemand Target where
    genDemandBudget _ _ = pure 0
    shrinkDemandCompatible (Target value) demanded =
        let observe thunk = M.tval thunk
            unused = M.tval value
        in [demanded]
