{-# LANGUAGE TypeFamilies #-}
module Generated where

import qualified Example as Source
import qualified Memorist.Memorist as M
import qualified Memorist.Example.List as ML
import qualified Memorist.Data.List as List

data BoxT = BoxT (M.Th Int)
data BoxD = BoxD (M.ThD Int)

instance M.Repr Source.Box where
    type Rep Source.Box = BoxT
    reprM (Source.Box value) = BoxT <$> M.lazy (pure value)
    abstract (BoxT field) = Source.Box (M.tval field)

instance M.Demand BoxT where
    type Dem BoxT = BoxD
    fullDemand (BoxT field) = BoxD (M.fullDemand field)
    usage (BoxT field) (BoxD mask) = M.usage field mask

keepM :: M.Th Int -> M.MemoristM Int
keepM x = M.tick >> M.force x

partialM :: M.Th (List.ListT Int) -> M.MemoristM (List.ListT Int)
partialM xs = do
    M.tick
    callback <- M.lazy (pure keepM)
    ML.mapMM callback xs
