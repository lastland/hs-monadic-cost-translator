module Case (probes) where

import Generated (identityM)
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalar "real" (\sample -> M.fromPure1 identityM (number sample))
    , scalar "dummy" (\sample -> let identityM = Source.double in pure (identityM (number sample)))
    ]
