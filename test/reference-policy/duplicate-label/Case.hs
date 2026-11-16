module Case (probes) where

import Generated
import ReferenceSupport

probes :: [Probe]
probes = [scalar "same" (const undefined), integer "same" (const undefined)]
