module Case (probes) where

import Generated
import ReferenceSupport

probes :: [Probe]
probes = [scalar "value" (const undefined)]
  where
    scalar _ _ = undefined
