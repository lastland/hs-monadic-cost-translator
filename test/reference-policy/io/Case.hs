module Case (probes) where

import Generated
import ReferenceSupport

probes :: [Probe]
probes = [scalar "value" (const undefined)]

effect :: IO ()
effect = pure ()
