{-# LANGUAGE TypeFamilies #-}
module BoxSupportAccounting where

import qualified Memorist.Memorist as M

data BoxT = BoxT (M.Th Int)
data BoxD = BoxD (M.ThD Int)

instance M.Demand BoxT where
    type Dem BoxT = BoxD
    fullDemand (BoxT x) = BoxD (M.fullDemand x)
    usage (BoxT _) (BoxD M.Skip) = M.empty
    usage (BoxT x) (BoxD (M.Need d)) =
        case M.useThunk x of (value, costs) -> costs `M.union` M.usage value d
