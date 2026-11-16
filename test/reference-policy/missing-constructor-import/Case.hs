module Case (probes) where

import Generated
import ReferenceSupport (Probe)

probes :: [Probe]
probes = [scalar "value" (const undefined)]
