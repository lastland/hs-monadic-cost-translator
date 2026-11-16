module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "shared-forwarded" (Source.shared . number) (\s -> M.fromPure1 sharedM (number s))
    , scalar "different-arguments" (\s -> M.fromPure2 addM (number s) (other s))
    , scalar "computed-forwarded-input" $ \s -> do
        x <- M.lazy (M.fromPure1 workM (number s))
        sharedM x
    , failure "demanded-bottom" (\_ -> M.fromPure1 sharedM (undefined :: Int))
    ]
