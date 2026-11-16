module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "filtered-lazy-tail" (Source.filtered . number) (\s -> M.fromPure1 filteredM (number s))
    , scalar "other-head" (\s -> M.fromPure1 filteredM (other s))
    , boolean "predicate-ignores-bottom" (\_ -> M.fromPure1 keepM (undefined :: Int))
    , failure "head-is-demanded" (\_ -> M.fromPure1 filteredM (undefined :: Int))
    ]
