{-# LANGUAGE KindSignatures, FlexibleContexts, FlexibleInstances, StandaloneDeriving,
    UndecidableInstances #-}
module StockHigherSpecialized where
import Data.Kind (Type)
import qualified Memorist.Memorist as M

data HigherD (f :: Type -> Type) a = HigherD (M.ThD (f a)) deriving Show
deriving instance {-# OVERLAPPING #-} Eq a => Eq (HigherD M.MaybeD a)
instance {-# OVERLAPPABLE #-} Eq (f a) => Eq (HigherD f a) where
    HigherD x == HigherD y = x == y

specialized :: HigherD M.MaybeD Int -> HigherD M.MaybeD Int -> Bool
specialized = (==)
manual :: HigherD M.ThD Bool -> HigherD M.ThD Bool -> Bool
manual = (==)

data RepeatedD (f :: Type -> Type) (g :: Type -> Type) a = RepeatedD (M.ThD (f a))
deriving instance {-# OVERLAPPING #-} Eq (f a) => Eq (RepeatedD f f a)
instance {-# OVERLAPPABLE #-} Eq (f a) => Eq (RepeatedD f g a) where
    RepeatedD x == RepeatedD y = x == y
repeated :: RepeatedD M.MaybeD M.MaybeD Int -> RepeatedD M.MaybeD M.MaybeD Int -> Bool
repeated = (==)
repeatedManual :: RepeatedD M.MaybeD M.ThD Int -> RepeatedD M.MaybeD M.ThD Int -> Bool
repeatedManual = (==)
