module Case (probes) where

import Generated (initialize, asIntM)
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource)

probes :: [Probe]
probes =
    [ scalarSource "fixed-same-dictionary-calls"
        (\_ -> Source.asInt 0 + Source.asInt 0) $ \_ -> do
        bindings <- initialize
        i <- M.fromPure1 (asIntM bindings) (0 :: Int)
        j <- M.fromPure1 (asIntM bindings) (0 :: Int)
        pure (i + j)
    , scalarSource "repeated-same-dictionary-pairs"
        (\sample -> sum [Source.asInt (number sample + step)
            + Source.asInt (other sample - step)
            | step <- [0 .. repeats sample - 1]]) $ \sample -> do
        bindings <- initialize
        sum <$> mapM (\step -> do
            i <- M.fromPure1 (asIntM bindings) (number sample + step)
            j <- M.fromPure1 (asIntM bindings) (other sample - step)
            pure (i + j)) [0 .. repeats sample - 1]
    , scalarSource "repeated-same-dictionary"
        (\sample -> sum [Source.asInt (number sample + step)
            | step <- [0 .. repeats sample - 1]]) $ \sample -> do
        bindings <- initialize
        sum <$> mapM (M.fromPure1 (asIntM bindings) . (number sample +))
            [0 .. repeats sample - 1]
    ]
