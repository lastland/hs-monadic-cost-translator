module Case (probes) where

import Generated (observedM)
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource)

probes :: [Probe]
probes =
    [ scalarSource "fixed-ignored-scoped-expression"
        (\_ -> Source.observed (5 :: Int)) $ \_ ->
        M.fromPure1 observedM (5 :: Int)
    , scalarSource "repeated-ignored-scoped-expression"
        (\sample -> sum [Source.observed (number sample + step)
            | step <- [0 .. repeats sample - 1]]) $ \sample ->
        sum <$> mapM (M.fromPure1 observedM . (number sample +))
            [0 .. repeats sample - 1]
    ]
