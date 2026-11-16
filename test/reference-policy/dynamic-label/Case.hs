module Case (probes) where

import Generated
import ReferenceSupport

probes :: [Probe]
probes = [scalar (show (number (undefined :: Sample))) (const undefined)]
