module Case (probes) where

import Generated
import ReferenceSupport
import qualified Memorist.Memorist as M

probes :: [Probe]
probes = concat
    [ [scalar "first" (\sample -> M.fromPure1 identityM (number sample))]
    , [scalar "second" (\sample -> M.fromPure1 identityM (other sample))]
    ]
