module Case (probes) where

import Generated
import qualified DemandSupport as D
import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as QC
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalar "function" (\sample -> M.fromPure1 identityM (number sample))
    , generatedDemands $ D.demandProbe "generator"
        (\sample -> M.fromPure1 identityM (number sample))
        (\budget value -> QC.resize budget (genIdentityDemand budget value))
        (\_ _ -> []) (D.toDemandCodec D.intPlan (const 0))
    ]
