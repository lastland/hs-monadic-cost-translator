{-# LANGUAGE TypeFamilies #-}
module Generated where

import Memorist.Memorist (Demand(..))
import qualified Memorist.Memorist as M

data BoxT a = BoxT (M.Th a) (M.Th Int)

data BoxD a = BoxD (M.ThD a) (M.ThD Int)

instance M.Demand a => M.Demand (BoxT a) where
    type Dem (BoxT a) = BoxD (M.Dem a)
    fullDemand (BoxT x n) = BoxD (M.fullDemand x) (M.fullDemand n)
    usage (BoxT x n) (BoxD dx dn) = M.union (M.usage x dx) (M.usage n dn)

wrapM :: M.Th a -> M.MemoristM (BoxT a)
wrapM x = do
    M.tick
    unused <- M.lazy (error "undefined primitive field")
    pure (BoxT x unused)

unboxM :: M.Th (BoxT a) -> M.MemoristM a
unboxM box = do
    M.tick
    BoxT x _ <- M.force box
    M.force x

boxedM :: M.Th a -> M.MemoristM a
boxedM x = do
    M.tick
    box <- M.lazy (wrapM x)
    unboxM box
