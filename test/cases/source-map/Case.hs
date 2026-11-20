module Case (probes) where

import Generated
import ReferenceSupport
import qualified Memorist.Memorist as M
import qualified Memorist.Data.List as ML

probes :: [Probe]
probes =
    [ scalar "keep" (\s -> M.fromPure1 keepM (number s))
    , demanded "map" (\_ -> M.abstract :: ML.ListT Int -> [Int]) (\_ -> M.fullDemand)
          (\s -> M.fromPure1 partialM [number s, other s])
    ]
