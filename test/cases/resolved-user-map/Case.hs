module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "user-map" (Source.wrapped . number) (\s -> M.fromPure1 wrappedM (number s))
    , scalar "user-map-other" (\s -> M.fromPure1 wrappedM (other s))
    , scalar "shared-map-input" $ \s -> do
        x <- M.lazy (M.fromPure1 wrappedM (number s))
        wrappedM x
    , failure "demanded-bottom" (\_ -> M.fromPure1 wrappedM (undefined :: Int))
    ]
