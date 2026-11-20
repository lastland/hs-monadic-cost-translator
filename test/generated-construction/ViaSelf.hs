{-# LANGUAGE PolyKinds, DataKinds, FlexibleInstances, StandaloneDeriving,
    DerivingStrategies, ExplicitForAll, DerivingVia, UndecidableInstances #-}
module ViaSelf where
import Data.Kind (Type)
import qualified Memorist.Memorist as M

data SelfD (a :: k) = S (M.ThD Int)
deriving stock instance {-# OVERLAPPING #-} forall (a :: Type). Eq (SelfD a)
deriving stock instance Show (SelfD a)
deriving via (SelfD a) instance {-# OVERLAPPABLE #-} Eq (SelfD a)

viaEq :: SelfD 'True -> SelfD 'True -> Bool
viaEq = (==)
stockEq :: SelfD Int -> SelfD Int -> Bool
stockEq = (==)
stockShow :: SelfD 'True -> String
stockShow = show
