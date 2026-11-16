module Case (probes) where

import Generated
import ReferenceSupport hiding (scalar)

probes :: [Probe]
probes = [scalar "value" (const undefined)]
