module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "paired"
        (\sample -> Source.paired (number sample))
        (\sample -> M.fromPure1 pairedM (number sample))
    ]
