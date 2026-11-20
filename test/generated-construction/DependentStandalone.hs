{-# LANGUAGE PolyKinds, DataKinds, StandaloneDeriving, DerivingStrategies,
    ExplicitForAll #-}
module DependentStandalone where
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import qualified Memorist.Memorist as M

data DependentD (a :: k) (f :: k -> Type) (b :: f a) = D (M.ThD Int)
deriving stock instance forall k (a :: k) (f :: k -> Type) (b :: f a).
    Eq (DependentD a f b)
deriving stock instance forall k (a :: k) (f :: k -> Type) (b :: f a).
    Show (DependentD a f b)
dependentEq :: DependentD 'True Proxy 'Proxy -> DependentD 'True Proxy 'Proxy -> Bool
dependentEq = (==)
dependentShow :: DependentD Int Maybe 'Nothing -> String
dependentShow = show
