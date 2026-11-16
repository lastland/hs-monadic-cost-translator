module Case (probes) where

import Generated (initialize, observedM)
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource)

probes :: [Probe]
probes =
    [ scalarSource "fixed-int-specialization"
        (\_ -> Source.observed (0 :: Int)) $ \_ -> do
        bindings <- initialize
        M.fromPure1 (observedM bindings) (0 :: Int)
    , scalarSource "undefined-scoped-argument"
        (\_ -> Source.observed (undefined :: Int)) $ \_ -> do
        bindings <- initialize
        M.fromPure1 (observedM bindings) (undefined :: Int)
    , scalarSource "repeated-shared-specialization"
        (\sample -> sum [Source.observed (number sample + step)
            | step <- [0 .. repeats sample - 1]]) $ \sample -> do
        bindings <- initialize
        sum <$> mapM (M.fromPure1 (observedM bindings) . (number sample +))
            [0 .. repeats sample - 1]
    ]
