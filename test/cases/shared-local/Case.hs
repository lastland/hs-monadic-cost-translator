module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "shared" (Source.shared . number) (\s -> M.fromPure1 sharedM (number s))
    , scalar "shared-other" (\s -> M.fromPure1 sharedM (other s))
    , scalar "shared-computed-input" $ \s -> do
        x <- M.lazy (M.fromPure1 doubleM (number s))
        sharedM x
    , failure "demanded-bottom" (\_ -> M.fromPure1 sharedM (undefined :: Int))
    ]
