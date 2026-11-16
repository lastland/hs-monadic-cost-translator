module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "observed"
        (\sample -> Source.observed (number sample))
        (\sample -> M.fromPure1 observedM (number sample))
    , scalarSource "choose"
        (\sample -> Source.choose (number sample))
        (\sample -> M.fromPure1 chooseM (number sample))
    ]
