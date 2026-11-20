{-# LANGUAGE PolyKinds, DataKinds, FlexibleInstances, StandaloneDeriving,
    DerivingStrategies, ExplicitForAll, TemplateHaskell #-}
module THFields where
import Data.Kind (Type)
import qualified Memorist.Memorist as M
import ManualGenerator (manualEq)

data Specialized (a :: k) = S (M.ThD Int)
deriving stock instance {-# OVERLAPPING #-} forall (a :: Type). Eq (Specialized a)
deriving stock instance Show (Specialized a)
$(manualEq ''Specialized True)

manualPoly :: Specialized 'True -> Specialized 'True -> Bool
manualPoly = (==)
stockSpecialized :: Specialized Int -> Specialized Int -> Bool
stockSpecialized = (==)
unrelatedShow :: Specialized 'True -> String
unrelatedShow = show
