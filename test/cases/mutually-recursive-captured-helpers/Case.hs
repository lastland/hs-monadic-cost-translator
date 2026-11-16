module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "mutually-recursive-capture" (Source.mutual . number) (\s -> M.fromPure1 mutualM (number s))
    , scalar "other-capture" (\s -> M.fromPure1 mutualM (other s))
    , scalar "computed-capture" $ \s -> do
        x <- M.lazy (M.fromPure1 mutualM (number s))
        mutualM x
    , failure "captured-bottom" (\_ -> M.fromPure1 mutualM (undefined :: Int))
    ]
