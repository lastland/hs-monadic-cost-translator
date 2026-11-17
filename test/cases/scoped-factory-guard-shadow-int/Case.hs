module Case (probes) where

import Generated (observedM)
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource)

probes :: [Probe]
probes =
    [ scalarSource "fixed-int-specialization"
        (\_ -> Source.observed (0 :: Int)) $ \_ ->
        M.fromPure1 observedM (0 :: Int)
    , scalarSource "undefined-scoped-argument"
        (\_ -> Source.observed (undefined :: Int)) $ \_ ->
        M.fromPure1 observedM (undefined :: Int)
    , scalarSource "repeated-shared-specialization"
        (\sample -> sum [Source.observed (number sample + step)
            | step <- [0 .. repeats sample - 1]]) $ \sample ->
        sum <$> mapM (M.fromPure1 observedM . (number sample +))
            [0 .. repeats sample - 1]
    ]
