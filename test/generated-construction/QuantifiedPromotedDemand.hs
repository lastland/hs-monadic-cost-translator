{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, PolyKinds,
    QuantifiedConstraints, RankNTypes, ScopedTypeVariables, StandaloneKindSignatures,
    TypeFamilies, TypeOperators, UndecidableInstances, UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module QuantifiedPromotedDemand where
import Data.Kind
import Data.Proxy
import qualified Memorist.Memorist as M
import QuantifiedPromoted (Index(..))

type DemOf :: forall k. k -> k
type DemOf (a :: k) = DemK k a
type DemK :: forall k -> k -> k
type family DemK k a where
    DemK Type a = M.Dem a
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

data HigherT (f :: Index -> Type) = HigherT (M.Th (f 'Key))
data HigherD (f :: Index -> Type) = HigherD (M.ThD (f 'Key))
instance forall f. DemandK f => M.Demand (HigherT f) where
    type Dem (HigherT f) = HigherD (DemOf f)
    fullDemand (HigherT x) = withConstraint (Proxy :: Proxy (DemandApp f 'Key))
        (HigherD (M.fullDemand x))
    usage (HigherT x) (HigherD d) = withConstraint (Proxy :: Proxy (DemandApp f 'Key))
        (M.usage x d)
