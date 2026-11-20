{-# LANGUAGE TypeFamilies #-}
module Generated (BoxT(..), BoxD(..)) where

import qualified Example as Source
import qualified Memorist.Memorist as M

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
