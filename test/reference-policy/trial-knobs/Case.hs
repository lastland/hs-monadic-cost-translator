module Case (probes) where

import Generated
import ReferenceSupport

probes :: [Probe]
probes = [scalar "value" (const undefined)]

maxSuccess = 0
disabled = quickCheckWith maxSuccess
