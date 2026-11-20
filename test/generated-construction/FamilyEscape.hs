{-# LANGUAGE ConstraintKinds, FlexibleContexts, TypeFamilies #-}
module FamilyEscape where

import Data.Kind (Type)
import FamilySupport
import qualified Memorist.Memorist as M

ordinaryRepr :: ReprReq Type a => Box a -> M.MemoristM (BoxT (M.Rep a))
ordinaryRepr = M.reprM

ordinaryAbstract :: ReprReq Type a => M.MaybeT (M.Rep a) -> Maybe a
ordinaryAbstract = M.abstract

ordinaryFull :: DemandReq Type a => M.Th (M.MaybeT a) -> M.ThD (M.MaybeD (M.Dem a))
ordinaryFull = M.fullDemand

ordinaryUsage :: DemandReq Type a => M.Th (M.MaybeT a) -> M.ThD (M.MaybeD (M.Dem a)) -> M.USet
ordinaryUsage = M.usage

class Foreign a where
    abstract :: M.Th a -> a
instance Foreign Int where
    abstract = M.tval
