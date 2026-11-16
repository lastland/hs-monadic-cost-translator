module Case (probes) where

import Generated
import ReferenceSupport

probes :: Sample -> [Probe]
probes sample = if number sample == 0 then [] else [scalar "value" (const undefined)]
