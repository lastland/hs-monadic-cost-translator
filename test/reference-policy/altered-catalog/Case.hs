module Case (probes) where

import Generated
import ReferenceSupport

probes :: [Probe]
probes = if True then [] else [scalar "value" (const undefined)]
