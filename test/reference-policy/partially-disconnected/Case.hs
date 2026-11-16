module Case (probes) where

import Generated (identityM)
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalar "real" (\sample -> M.fromPure1 identityM (number sample))
    , scalarSource "dummy" (Source.double . number) (pure . Source.double . number)
    ]
