{-# LANGUAGE FlexibleInstances, StandaloneDeriving #-}
module StockSpecialized where

data SpecialD a = SpecialD a
deriving instance {-# OVERLAPPING #-} Eq (SpecialD Int)
instance {-# OVERLAPPABLE #-} Eq a => Eq (SpecialD a) where
    SpecialD x == SpecialD y = x == y

manual :: SpecialD Bool -> SpecialD Bool -> Bool
manual = (==)
