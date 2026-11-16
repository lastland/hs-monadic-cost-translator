module Case (probes) where

import Generated
import qualified Example as Source
import ReferenceSupport

probes :: [Probe]
probes = [scalarSource "dummy" (Source.double . number) (pure . Source.double . number)]
