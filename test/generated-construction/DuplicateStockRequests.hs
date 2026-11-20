{-# LANGUAGE PolyKinds, FlexibleContexts, StandaloneDeriving, DerivingStrategies,
    UndecidableInstances #-}
module DuplicateStockRequests where
import Data.Kind (Type)
import qualified Memorist.Memorist as M

data DoubleD (f :: k -> Type) (a :: k) = D (M.ThD (f a))
deriving stock instance {-# OVERLAPPABLE #-} Eq (f a) => Eq (DoubleD f a)
deriving stock instance {-# OVERLAPPING #-} (Eq (f a), Show (f a)) => Eq (DoubleD f a)
