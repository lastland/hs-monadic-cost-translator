{-# LANGUAGE PolyKinds, DataKinds, FlexibleInstances, StandaloneDeriving,
    DerivingStrategies, ExplicitForAll, TypeApplications #-}
module PolyHeads where
import Data.Kind (Type)
import qualified Memorist.Memorist as M

data Specialized (a :: k) = S (M.ThD Int)
deriving stock instance forall (a :: Type). Eq (Specialized a)
instance {-# OVERLAPPABLE #-} Eq (Specialized a) where
    _ == _ = True
deriving stock instance Show (Specialized a)
specializedKind :: Specialized Int -> Specialized Int -> Bool
specializedKind = (==)
manualPoly :: Specialized 'True -> Specialized 'True -> Bool
manualPoly = (==)

data Repeated (a :: k) (b :: l) = R (M.ThD Int)
deriving stock instance forall k (a :: k) (b :: k). Eq (Repeated a b)
instance {-# OVERLAPPABLE #-} Eq (Repeated a b) where
    _ == _ = True
deriving stock instance Show (Repeated a b)
repeatedKind :: Repeated 'True 'False -> Repeated 'True 'False -> Bool
repeatedKind = (==)
manualRepeated :: Repeated 'True Int -> Repeated 'True Int -> Bool
manualRepeated = (==)

data Visible (a :: k) = V (M.ThD Int)
deriving stock instance Eq (Visible @Type a)
instance {-# OVERLAPPABLE #-} Eq (Visible a) where
    _ == _ = True
deriving stock instance Show (Visible a)
visibleSpecialized :: Visible Int -> Visible Int -> Bool
visibleSpecialized = (==)
visibleManual :: Visible 'True -> Visible 'True -> Bool
visibleManual = (==)

data Wrong (a :: k) = W (M.ThD Int) deriving Show
instance Eq (Wrong a) where
    _ == _ = True
wrongClass :: Wrong 'True -> Wrong 'True -> Bool
wrongClass = (==)

data Empty (a :: k) = E (M.ThD Int)
deriving stock instance forall (a :: Type). Eq (Empty a)
instance {-# OVERLAPPABLE #-} Eq (Empty a)
deriving stock instance Show (Empty a)
emptyManual :: Empty 'True -> Empty 'True -> Bool
emptyManual = (==)
