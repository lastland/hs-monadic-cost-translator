module Case (probes) where

import Generated (oneM, twoM)
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource)

probes :: [Probe]
probes =
    [ scalarSource "fixed-all-dictionary-keys"
        (\_ -> Source.one 0 + Source.two 0) $ \_ -> do
        i <- M.fromPure1 oneM (0 :: Int)
        j <- M.fromPure1 twoM (0 :: Int)
        pure (i + j)
    , scalarSource "interleaved-all-dictionary-keys"
        (\sample -> sum [Source.one (number sample + step)
            + Source.two (other sample - step)
            | step <- [0 .. repeats sample - 1]]) $ \sample ->
        sum <$> mapM (\step -> do
            let first = M.fromPure1 oneM (number sample + step)
                second = M.fromPure1 twoM (other sample - step)
            if even (variant sample + step) then do
                i <- first
                j <- second
                pure (i + j)
            else do
                j <- second
                i <- first
                pure (i + j)) [0 .. repeats sample - 1]
    , scalarSource "repeated-first-dictionary-key"
        (\sample -> sum [Source.one (number sample + step)
            | step <- [0 .. repeats sample - 1]]) $ \sample ->
        sum <$> mapM (M.fromPure1 oneM . (number sample +))
            [0 .. repeats sample - 1]
    , scalarSource "repeated-second-dictionary-key"
        (\sample -> sum [Source.two (other sample - step)
            | step <- [0 .. repeats sample - 1]]) $ \sample ->
        sum <$> mapM (M.fromPure1 twoM . (other sample -))
            [0 .. repeats sample - 1]
    ]
