module Case (probes) where

import ReferenceSupport

probes :: [Probe]
probes = [scalar "value" (const undefined)]
