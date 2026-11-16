module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "guarded" (Source.guarded . number) (\s -> M.fromPure1 guardedM (number s))
    , scalar "positive-guard" (\s -> M.fromPure1 guardedM (abs (other s) + 1))
    , failure "negative-guard" (\s -> M.fromPure1 guardedM (negate (abs (number s) + 1)))
    , failure "zero-guard" (\_ -> M.fromPure1 guardedM (0 :: Int))
    , failure "bottom-guard" (\_ -> M.fromPure1 guardedM (undefined :: Int))
    ]
