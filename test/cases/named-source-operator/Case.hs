module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "operated"
        (\sample -> Source.operated (number sample))
        (\sample -> M.fromPure1 operatedM (number sample))
    ]
