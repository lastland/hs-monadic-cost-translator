module Case (probes) where

import Generated
import qualified Example as Source
import Memorist.Data.List ()
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "ordered" (Source.ordered . number) (\s -> M.fromPure1 orderedM (number s))
    , scalar "direct-lazy-tail" (\s -> M.fromPure1 chooseM (other s : undefined))
    , failure "empty-equation" (\_ -> M.fromPure1 chooseM ([] :: [Int]))
    , failure "bottom-spine" (\_ -> M.fromPure1 chooseM (undefined :: [Int]))
    , failure "bottom-head" (\_ -> M.fromPure1 orderedM (undefined :: Int))
    ]
