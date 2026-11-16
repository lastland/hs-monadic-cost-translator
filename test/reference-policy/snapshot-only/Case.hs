module Case (probes) where

import Generated
import ReferenceSupport

probes :: [Probe]
probes = [snapshot "not-a-cost-check" (const True)]
