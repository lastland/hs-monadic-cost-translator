module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "unused-error-and-short-circuit" (Source.keep . number) (\s -> M.fromPure1 keepM (number s))
    , scalar "other-input" (\s -> M.fromPure1 keepM (other s))
    , scalar "shared-computed-input" $ \s -> do
        x <- M.lazy (M.fromPure1 keepM (number s))
        keepM x
    , failure "returned-bottom" (\_ -> M.fromPure1 keepM (undefined :: Int))
    ]
