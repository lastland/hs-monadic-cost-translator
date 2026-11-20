{-# LANGUAGE PolyKinds, DataKinds, StandaloneDeriving, DerivingStrategies,
    PartialTypeSignatures #-}
module WildcardRequest where
import qualified Memorist.Memorist as M

data WildD (a :: k) = W (M.ThD Int)
deriving stock instance _ => Eq (WildD a)
deriving stock instance _ => Show (WildD a)
wildEq :: WildD 'True -> WildD 'True -> Bool
wildEq = (==)
