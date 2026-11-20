{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, QuantifiedConstraints,
    RankNTypes, ScopedTypeVariables, TypeFamilies, UndecidableInstances, UndecidableSuperClasses #-}
module QuantifiedForeign where
import Data.Proxy
import qualified Memorist.Memorist as M

class Repr a where
    reprM :: M.Memorist s m => a -> m a
    abstract :: a -> a
class (forall a. Repr a => Repr (f a)) => ReprF f
instance (forall a. Repr a => Repr (f a)) => ReprF f
withConstraint :: c => Proxy c -> (c => r) -> r
withConstraint _ body = body

data Box f a = Box (f a)
instance forall f a. (ReprF f, Repr a) => M.Repr (Box f a) where
    type Rep (Box f a) = Box f a
    reprM (Box x) = withConstraint (Proxy :: Proxy (Repr (f a))) (Box <$> reprM x)
    abstract (Box x) = withConstraint (Proxy :: Proxy (Repr (f a))) (Box (abstract x))
