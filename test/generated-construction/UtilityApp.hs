{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, PolyKinds,
    QuantifiedConstraints, RankNTypes, ScopedTypeVariables, StandaloneKindSignatures,
    TypeFamilies, TypeOperators, UndecidableInstances, UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module UtilityApp where
import Data.Kind
import Data.Proxy
import qualified Memorist.Memorist as M

-- Same nominal application/superclass/family route as publication 080.
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

type DemOf :: forall k. k -> k
type DemOf (a :: k) = DemK k a
type DemK :: forall k -> k -> k
type family DemK k a where
    DemK Type a = M.Dem a
    DemK (Type -> Type) M.MaybeT = M.MaybeD
    DemK k (f (a :: j)) = DemK (j -> k) f (DemK j a)
    DemK k a = a
type DemandReq :: forall k -> k -> Constraint
type family DemandReq k a where
    DemandReq Type a = M.Demand a
    DemandReq (k -> l) f = DemandF f
    DemandReq k a = ()
type DemandK :: forall k. k -> Constraint
type DemandK (a :: k) = DemandReq k a
type DemandApp :: forall k l. (k -> l) -> k -> Constraint
class (DemandK (f a), DemOf (f a) ~ DemOf f (DemOf a)) => DemandApp f a
instance (DemandK (f a), DemOf (f a) ~ DemOf f (DemOf a)) => DemandApp f a
type DemandF :: forall k l. (k -> l) -> Constraint
class (forall a. DemandK a => DemandApp f a) => DemandF f
instance (forall a. DemandK a => DemandApp f a) => DemandF f

withConstraint :: c => Proxy c -> (c => r) -> r
withConstraint _ body = body

-- Fixed Maybe tests utility permission and concrete reprM replay, not ReprF.
data Box a = Box (Maybe a)
instance forall a. ReprK a => M.Repr (Box a) where
    type Rep (Box a) = M.MaybeT (M.Rep a)
    reprM (Box x) = M.reprM x
    abstract x = withConstraint (Proxy :: Proxy (ReprApp Maybe a))
        (Box (M.abstract x))
makeInt :: Box Int -> M.MemoristM (M.MaybeT Int)
makeInt = M.reprM
makeBool :: Box Bool -> M.MemoristM (M.MaybeT Bool)
makeBool = M.reprM

data HigherT (f :: Type -> Type) (a :: Type) =
    HigherT { scalar :: M.Th a, applied :: M.Th (f a) }
data HigherD (f :: Type -> Type) (a :: Type) = HigherD (M.ThD a) (M.ThD (f a))
instance forall f a. (DemandK f, DemandK a) => M.Demand (HigherT f a) where
    type Dem (HigherT f a) = HigherD (DemOf f) (DemOf a)
    fullDemand (HigherT x y) =
        withConstraint (Proxy :: Proxy (DemandApp f a))
            (HigherD (M.fullDemand x) (M.fullDemand y))
    usage (HigherT x y) (HigherD dx dy) =
        withConstraint (Proxy :: Proxy (DemandApp f a))
            (M.usage x dx `M.union` (M.usage y dy `M.union` M.empty))
