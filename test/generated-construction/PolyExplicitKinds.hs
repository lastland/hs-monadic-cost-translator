{-# LANGUAGE PolyKinds, DataKinds, StandaloneDeriving, DerivingStrategies,
    ExplicitForAll, TypeApplications #-}
module PolyExplicitKinds where
import qualified Memorist.Memorist as M

data ExplicitD (a :: k) = E (M.ThD Int)
deriving stock instance forall k (a :: k). Eq (ExplicitD @k a)
deriving stock instance forall k (a :: k). Show (ExplicitD @k a)
explicitEq :: ExplicitD 'True -> ExplicitD 'True -> Bool
explicitEq = (==)
explicitShow :: ExplicitD 'False -> String
explicitShow = show
