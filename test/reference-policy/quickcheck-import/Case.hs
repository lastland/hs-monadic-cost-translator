module Case (probes) where

import Generated
import ReferenceSupport
import Test.QuickCheck

probes :: [Probe]
probes = [scalar "value" (const undefined)]
