{-# LANGUAGE KindSignatures, FlexibleContexts, FlexibleInstances, StandaloneDeriving,
    DerivingStrategies, DerivingVia, UndecidableInstances #-}
module StockHigherVia where
import Data.Kind (Type)
import qualified Memorist.Memorist as M

data HigherD (f :: Type -> Type) a = HigherD (M.ThD (f a)) deriving Show
newtype ViaD (f :: Type -> Type) a = ViaD (HigherD f a)
instance Eq (f a) => Eq (ViaD f a) where
    ViaD (HigherD x) == ViaD (HigherD y) = x == y
deriving via (ViaD f a) instance Eq (f a) => Eq (HigherD f a)

viaEq :: HigherD M.MaybeD Int -> HigherD M.MaybeD Int -> Bool
viaEq = (==)
