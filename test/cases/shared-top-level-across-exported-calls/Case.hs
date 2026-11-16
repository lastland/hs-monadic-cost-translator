module Case (probes) where

import Generated (initialize, firstM, secondM)
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource)

probes :: [Probe]
probes =
    [ scalarSource "fixed-exported-calls"
        (\_ -> Source.first (5 :: Int) + Source.second 5) $ \_ -> do
        bindings <- initialize
        a <- M.fromPure1 (firstM bindings) (5 :: Int)
        b <- M.fromPure1 (secondM bindings) (5 :: Int)
        pure (a + b)
    , scalarSource "interleaved-exported-calls"
        (\sample -> sum [Source.first (number sample + step)
            + Source.second (other sample - step)
            | step <- [0 .. repeats sample - 1]]) $ \sample -> do
        bindings <- initialize
        sum <$> mapM (\step -> do
            let first = M.fromPure1 (firstM bindings) (number sample + step)
                second = M.fromPure1 (secondM bindings) (other sample - step)
            if even (variant sample + step) then do
                a <- first
                b <- second
                pure (a + b)
            else do
                b <- second
                a <- first
                pure (a + b)) [0 .. repeats sample - 1]
    , scalarSource "repeated-first-call"
        (\sample -> sum [Source.first (number sample + step)
            | step <- [0 .. repeats sample - 1]]) $ \sample -> do
        bindings <- initialize
        sum <$> mapM (M.fromPure1 (firstM bindings) . (number sample +))
            [0 .. repeats sample - 1]
    ]
