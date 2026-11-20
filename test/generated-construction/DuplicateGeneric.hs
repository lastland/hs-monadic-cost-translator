{-# LANGUAGE PolyKinds, FlexibleInstances, StandaloneDeriving,
    DerivingStrategies, UndecidableInstances #-}
module DuplicateGeneric where
import qualified Memorist.Memorist as M

data DuplicateD (a :: k) = D (M.ThD Int)
deriving stock instance Eq (DuplicateD a)
instance {-# OVERLAPPABLE #-} Show (DuplicateD a) => Eq (DuplicateD a) where
    _ == _ = True
