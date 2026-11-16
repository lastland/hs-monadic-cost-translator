module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "nested"
        (\sample -> Source.nested (number sample))
        (\sample -> M.fromPure1 nestedM (number sample))
    ]
