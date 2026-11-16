module Case (probes) where

import Generated (initialize, observedM)
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource)

probes :: [Probe]
probes =
    [ scalarSource "fixed-ignored-specialization"
        (\_ -> Source.observed (5 :: Int)) $ \_ -> do
        bindings <- initialize
        M.fromPure1 (observedM bindings) (5 :: Int)
    , scalarSource "repeated-ignored-specialization"
        (\sample -> sum [Source.observed (number sample + step)
            | step <- [0 .. repeats sample - 1]]) $ \sample -> do
        bindings <- initialize
        sum <$> mapM (M.fromPure1 (observedM bindings) . (number sample +))
            [0 .. repeats sample - 1]
    ]
