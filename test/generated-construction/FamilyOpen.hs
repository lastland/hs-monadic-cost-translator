{-# LANGUAGE ConstraintKinds, FlexibleContexts, PolyKinds, StandaloneKindSignatures,
    TypeFamilies, UndecidableInstances #-}
module FamilyOpen where

import Data.Kind (Constraint, Type)
import qualified Memorist.Memorist as M

type ReprReq :: forall k -> k -> Constraint
type family ReprReq k a
type instance ReprReq Type a = M.Repr a

data Box a = Box (Maybe a)
data BoxT a = BoxT (M.Th (M.MaybeT a))

instance ReprReq Type a => M.Repr (Box a) where
    type Rep (Box a) = BoxT (M.Rep a)
    reprM (Box x) = BoxT <$> M.lazy (M.reprM x)
    abstract (BoxT x) = Box (M.abstract (M.tval x))
