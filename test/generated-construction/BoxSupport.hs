{-# LANGUAGE TypeFamilies #-}
module BoxSupport where

import qualified Memorist.Memorist as M

data Box = Box Int
data BoxT = BoxT (M.Th Int)
data BoxD = BoxD (M.ThD Int)

instance M.Repr Box where
    type Rep Box = BoxT
    reprM (Box x) = BoxT <$> M.lazy (M.reprM x)
    abstract (BoxT x) = Box (M.abstract (M.tval x))

instance M.Demand BoxT where
    type Dem BoxT = BoxD
    fullDemand (BoxT x) = BoxD (M.fullDemand x)
    usage (BoxT x) (BoxD d) = M.usage x d `M.union` M.empty

ordinary :: M.Th Int -> M.MemoristM Int
ordinary x = M.force x
