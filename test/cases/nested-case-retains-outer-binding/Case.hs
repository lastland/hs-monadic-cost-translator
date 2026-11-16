module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "nested-cases" (Source.nested . number) (\s -> M.fromPure1 nestedM (number s))
    , scalar "other-input" (\s -> M.fromPure1 nestedM (other s))
    , scalar "outer-binding-shares-computed-input" $ \s -> do
        x <- M.lazy (M.fromPure1 nestedM (number s))
        nestedM x
    , failure "demanded-bottom" (\_ -> M.fromPure1 nestedM (undefined :: Int))
    ]
