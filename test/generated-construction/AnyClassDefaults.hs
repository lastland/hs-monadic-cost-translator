{-# LANGUAGE PolyKinds, DataKinds, FlexibleInstances, StandaloneDeriving,
    DerivingStrategies, ExplicitForAll, DeriveAnyClass, DefaultSignatures #-}
module AnyClassDefaults where
import Data.Kind (Type)
import qualified Memorist.Memorist as M

data AnyEq (a :: k) = AE (M.ThD Int)
deriving stock instance {-# OVERLAPPING #-} forall (a :: Type). Eq (AnyEq a)
deriving anyclass instance {-# OVERLAPPABLE #-} Eq (AnyEq a)
deriving stock instance Show (AnyEq a)
anyEq :: AnyEq 'True -> AnyEq 'True -> Bool
anyEq = (==)

data AnyShow (a :: k) = AS (M.ThD Int)
deriving stock instance {-# OVERLAPPING #-} forall (a :: Type). Show (AnyShow a)
deriving anyclass instance {-# OVERLAPPABLE #-} Show (AnyShow a)
deriving stock instance Eq (AnyShow a)
anyShow :: AnyShow 'True -> String
anyShow = show

class Local a where
    local :: a -> a -> Bool
    default local :: Show a => a -> a -> Bool
    local _ _ = True
deriving anyclass instance Local (AnyEq a)
localDefault :: AnyEq 'True -> AnyEq 'True -> Bool
localDefault = local
