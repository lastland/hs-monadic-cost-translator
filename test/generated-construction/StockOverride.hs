{-# LANGUAGE FlexibleContexts, FlexibleInstances, StandaloneDeriving, UndecidableInstances #-}
module StockOverride where

import qualified Memorist.Memorist as M

data OverrideD a = OverrideD (M.ThD a)
deriving instance {-# OVERLAPPABLE #-} Eq a => Eq (OverrideD a)
deriving instance {-# OVERLAPPABLE #-} Show a => Show (OverrideD a)

instance {-# OVERLAPPING #-} Eq (OverrideD Int) where
    _ == _ = True
    _ /= _ = M.tval (undefined :: M.Th Bool)
instance {-# OVERLAPPING #-} Show (OverrideD Int) where
    showsPrec _ _ = id
    show _ = show (M.tval (undefined :: M.Th Int))

sameOverride :: OverrideD Int -> OverrideD Int -> Bool
sameOverride = (==)
differentOverride :: OverrideD Int -> OverrideD Int -> Bool
differentOverride = (/=)
showOverride :: OverrideD Int -> String
showOverride = show
precOverride :: Int -> OverrideD Int -> ShowS
precOverride = showsPrec

sameStock :: OverrideD Bool -> OverrideD Bool -> Bool
sameStock = (==)
showStock :: OverrideD Bool -> String
showStock = show
