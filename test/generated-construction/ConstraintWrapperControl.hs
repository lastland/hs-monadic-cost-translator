{-# LANGUAGE ConstraintKinds, RankNTypes, TypeFamilies #-}
module ConstraintWrapperControl where

import Data.Proxy (Proxy(..))
import qualified DemandControl as D
import qualified Memorist.Memorist as M

-- The checked identity protocol, deliberately without the generated name.
permit :: c => Proxy c -> (c => r) -> r
permit _ body = body

data Source = Source Int
data Target = Target (M.Th Int)
data Observation = Observation (M.ThD Int)

instance M.Repr Source where
    type Rep Source = Target
    reprM (Source input) = permit (Proxy :: Proxy (M.Repr Int)) (Target <$> M.lazy (M.reprM input))
    abstract (Target field) = permit (Proxy :: Proxy (M.Repr Int)) (Source (M.abstract (M.tval field)))

instance M.Demand Target where
    type Dem Target = Observation
    fullDemand (Target field) = permit (Proxy :: Proxy (M.Demand Int)) (Observation (M.fullDemand field))
    usage (Target field) (Observation demanded) = permit (Proxy :: Proxy (M.Demand Int))
        (M.usage field demanded `M.union` M.empty)

instance D.Observation Target where
    generate size (Target field) = permit (Proxy :: Proxy (D.Observation Int))
        (Observation <$> D.generateField size field)
    shrinkCompatible (Target field) (Observation demanded) = permit (Proxy :: Proxy (D.Observation Int))
        [Observation smaller | smaller <- D.shrinkView field demanded]
