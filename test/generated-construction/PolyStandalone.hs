{-# LANGUAGE PolyKinds, FlexibleContexts, UndecidableInstances, DataKinds,
    StandaloneDeriving, DerivingStrategies, ExplicitForAll #-}
module PolyStandalone where
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import qualified Memorist.Memorist as M

data HigherD (f :: k -> Type) (a :: k) = HigherD (M.ThD (f a))
deriving stock instance forall k (f :: k -> Type) (a :: k).
    Eq (f a) => Eq (HigherD f a)
deriving stock instance forall k (f :: k -> Type) (a :: k).
    Show (f a) => Show (HigherD f a)
sameInt :: HigherD M.MaybeD Int -> HigherD M.MaybeD Int -> Bool
sameInt = (==)
sameBool :: HigherD M.MaybeD Bool -> HigherD M.MaybeD Bool -> Bool
sameBool = (/=)
renderInt :: HigherD M.MaybeD Int -> String
renderInt = show
precBool :: Int -> HigherD M.MaybeD Bool -> ShowS
precBool = showsPrec
listInt :: [HigherD M.MaybeD Int] -> ShowS
listInt = showList

data IndexedD (a :: k) = I (M.ThD Int)
deriving stock instance forall k (a :: k). Eq (IndexedD a)
deriving stock instance forall k (a :: k). Show (IndexedD a)
indexEq :: IndexedD 'True -> IndexedD 'True -> Bool
indexEq = (==)
indexShow :: IndexedD 'False -> String
indexShow = show

data DependentD (a :: k) (f :: k -> Type) (b :: f a) = D (M.ThD Int)
  deriving (Eq, Show)
dependentEq :: DependentD 'True Proxy 'Proxy -> DependentD 'True Proxy 'Proxy -> Bool
dependentEq = (==)
dependentShow :: DependentD Int Maybe 'Nothing -> String
dependentShow = show
