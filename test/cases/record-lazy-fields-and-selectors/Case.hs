module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "recorded"
        (\sample -> Source.recorded (number sample))
        (\sample -> M.fromPure1 recordedM (number sample))
    ]
