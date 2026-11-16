module Case (probes) where

import Generated
import qualified Example as Source
import ReferenceSupport

probes :: [Probe]
probes = concatMap build [("dummy", Source.double)]
  where
    build (label, make) = [scalarSource label (make . number) (pure . make . number)]
