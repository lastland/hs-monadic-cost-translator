{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, PolyKinds,
    QuantifiedConstraints, RankNTypes, ScopedTypeVariables, StandaloneKindSignatures,
    TypeFamilies, TypeOperators, UndecidableInstances, UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module QuantifiedPromoted where
import Data.Kind
import Data.Proxy
import qualified Memorist.Memorist as M

data Index = Key
type RepOf :: forall k. k -> k
type RepOf (a :: k) = RepK k a
type RepK :: forall k -> k -> k
type family RepK k a where
    RepK Type a = M.Rep a
    RepK k (f (a :: j)) = RepK (j -> k) f (RepK j a)
    RepK k a = a
type ReprReq :: forall k -> k -> Constraint
type family ReprReq k a where
    ReprReq Type a = M.Repr a
    ReprReq (k -> l) f = ReprF f
    ReprReq k a = ()
type ReprK :: forall k. k -> Constraint
type ReprK (a :: k) = ReprReq k a
type ReprApp :: forall k l. (k -> l) -> k -> Constraint
class (ReprK (f a), RepOf (f a) ~ RepOf f (RepOf a)) => ReprApp f a
instance (ReprK (f a), RepOf (f a) ~ RepOf f (RepOf a)) => ReprApp f a
type ReprF :: forall k l. (k -> l) -> Constraint
class (forall a. ReprK a => ReprApp f a) => ReprF f
instance (forall a. ReprK a => ReprApp f a) => ReprF f

withConstraint :: c => Proxy c -> (c => r) -> r
withConstraint _ body = body

data Higher (f :: Index -> Type) = Higher (f 'Key)
data HigherT (f :: Index -> Type) = HigherT (M.Th (f 'Key))
instance forall f. ReprK f => M.Repr (Higher f) where
    type Rep (Higher f) = HigherT (RepOf f)
    reprM (Higher x) = withConstraint (Proxy :: Proxy (ReprApp f 'Key))
        (HigherT <$> M.lazy (M.reprM x))
    abstract (HigherT x) = withConstraint (Proxy :: Proxy (ReprApp f 'Key))
        (Higher (M.abstract (M.tval x)))
