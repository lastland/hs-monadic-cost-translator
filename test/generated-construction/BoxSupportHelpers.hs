{-# LANGUAGE TypeFamilies #-}
module BoxSupportHelpers where

import qualified Memorist.Memorist as M

data Box = Box Int
data BoxT = BoxT (M.Th Int)
data BoxD = BoxD (M.ThD Int)

payload :: M.Th Int -> Int
payload x = M.tval x

instance M.Repr Box where
    type Rep Box = BoxT
    reprM (Box x) = BoxT <$> M.lazy (M.reprM x)
    abstract (BoxT x) = Box (payload x)

instance M.Demand BoxT where
    type Dem BoxT = BoxD
    fullDemand (BoxT x) = BoxD (M.Need (payload x))
    usage (BoxT x) (BoxD d) = M.usage x d `M.union` M.empty
