module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "double" (Source.double . number) (\s -> M.fromPure1 doubleM (number s))
    , scalar "double-other" (\s -> M.fromPure1 doubleM (other s))
    , scalar "shared-computed-argument" $ \s -> do
        x <- M.lazy (M.fromPure1 doubleM (number s))
        doubleM x
    , failure "demanded-bottom" (\_ -> M.fromPure1 doubleM (undefined :: Int))
    ]
