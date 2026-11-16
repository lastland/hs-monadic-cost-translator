module Case (probes) where

import Generated (identityM)
import ReferenceSupport (Sample(..), Probe, scalar)
import qualified Memorist.Memorist as M

probes :: [Probe]
probes = [scalar "value" make]
  where
    make sample = M.fromPure1 identityM (number sample)
