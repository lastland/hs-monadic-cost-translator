module Case (probes) where

import Generated (identityM)
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes = [scalar ("unbounded/" ++ show index) (\sample -> M.fromPure1 identityM (number sample)) | index <- [0 ..]]
