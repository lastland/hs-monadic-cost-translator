{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, PolyKinds,
    QuantifiedConstraints, RankNTypes, ScopedTypeVariables, StandaloneKindSignatures,
    TypeFamilies, TypeOperators, UndecidableInstances, UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module QuantifiedApplication where
import Data.Kind
import Data.Proxy
import qualified Memorist.Memorist as M

type RepOf :: forall k. k -> k
type RepOf (a :: k) = RepK k a
type RepK :: forall k -> k -> k
type family RepK k a where
    RepK Type a = M.Rep a
    RepK (Type -> Type) Maybe = M.MaybeT
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

data Higher f a = Higher a (f a)
data HigherT (f :: Type -> Type) (a :: Type) =
    HigherT { scalarField :: M.Th a, appliedField :: M.Th (f a) }
instance forall f a. (ReprK f, ReprK a) => M.Repr (Higher f a) where
    type Rep (Higher f a) = HigherT (RepOf f) (RepOf a)
    reprM (Higher scalar applied) = withConstraint (Proxy :: Proxy (ReprApp f a))
        ((HigherT <$> M.lazy (M.reprM scalar)) <*> M.lazy (M.reprM applied))
    abstract (HigherT scalar applied) = withConstraint (Proxy :: Proxy (ReprApp f a))
        (Higher (M.abstract (M.tval scalar)) (M.abstract (M.tval applied)))
