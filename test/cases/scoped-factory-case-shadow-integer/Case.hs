module Case (probes) where

import Generated (initialize, observedM)
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource)

probes :: [Probe]
probes =
    [ scalarSource "fixed-integer-specialization"
        (\_ -> Source.observed (0 :: Integer)) $ \_ -> do
        bindings <- initialize
        M.fromPure1 (observedM bindings) (0 :: Integer)
    , scalarSource "undefined-scoped-argument"
        (\_ -> Source.observed (undefined :: Integer)) $ \_ -> do
        bindings <- initialize
        M.fromPure1 (observedM bindings) (undefined :: Integer)
    , scalarSource "repeated-shared-specialization"
        (\sample -> sum [Source.observed (toInteger (number sample + step))
            | step <- [0 .. repeats sample - 1]]) $ \sample -> do
        bindings <- initialize
        sum <$> mapM (M.fromPure1 (observedM bindings) . toInteger . (number sample +))
            [0 .. repeats sample - 1]
    ]
