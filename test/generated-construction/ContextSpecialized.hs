{-# LANGUAGE PolyKinds, DataKinds, FlexibleInstances, FlexibleContexts,
    StandaloneDeriving, DerivingStrategies, UndecidableInstances #-}
module ContextSpecialized where
import Data.Kind (Type)
import Data.Proxy (Proxy)
import qualified Memorist.Memorist as M

data ContextD (f :: k -> Type) (a :: k) = CD (M.ThD (f a))
deriving stock instance {-# OVERLAPPING #-} (Eq a, Eq (f a)) => Eq (ContextD f a)
deriving stock instance Show (f a) => Show (ContextD f a)
instance {-# OVERLAPPABLE #-} Eq (ContextD f a) where
    _ == _ = True
contextManual :: ContextD Proxy 'True -> ContextD Proxy 'True -> Bool
contextManual = (==)
