module Case (probes) where

import Generated ()
import ReferenceSupport

probes :: [Probe]
probes = [scalar "dummy" (pure . number)]
