{-# LANGUAGE ConstraintKinds, FlexibleContexts, PolyKinds, StandaloneKindSignatures,
    TypeFamilies, UndecidableInstances #-}
module FamilySupport where

import Data.Kind (Constraint, Type)
import qualified Memorist.Memorist as M

type ReprReq :: forall k -> k -> Constraint
type family ReprReq k a where
    ReprReq Type a = M.Repr a
type DemandReq :: forall k -> k -> Constraint
type family DemandReq k a where
    DemandReq Type a = M.Demand a

data Box a = Box (Maybe a)
data BoxT a = BoxT (M.Th (M.MaybeT a))
data BoxD a = BoxD (M.ThD (M.MaybeD a))

instance ReprReq Type a => M.Repr (Box a) where
    type Rep (Box a) = BoxT (M.Rep a)
    reprM (Box x) = BoxT <$> M.lazy (M.reprM x)
    abstract (BoxT x) = Box (M.abstract (M.tval x))

instance DemandReq Type a => M.Demand (BoxT a) where
    type Dem (BoxT a) = BoxD (M.Dem a)
    fullDemand (BoxT x) = BoxD (M.fullDemand x)
    usage (BoxT x) (BoxD dx) = M.usage x dx

selected :: Box Int -> M.MemoristM (BoxT Int)
selected = M.reprM

selectedBool :: Box Bool -> M.MemoristM (BoxT Bool)
selectedBool = M.reprM
