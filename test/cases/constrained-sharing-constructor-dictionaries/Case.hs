module Case (probes) where

import Generated (initialize, asListM, asMaybeM)
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource)

probes :: [Probe]
probes =
    [ scalarSource "fixed-constructor-dictionaries-with-undefined-fields"
        (\_ -> Source.asList 0 + Source.asMaybe 0) $ \_ -> do
        bindings <- initialize
        i <- M.fromPure1 (asListM bindings) (0 :: Int)
        j <- M.fromPure1 (asMaybeM bindings) (0 :: Int)
        pure (i + j)
    , scalarSource "interleaved-constructor-dictionaries"
        (\sample -> sum [Source.asList (number sample + step)
            + Source.asMaybe (other sample - step)
            | step <- [0 .. repeats sample - 1]]) $ \sample -> do
        bindings <- initialize
        sum <$> mapM (\step -> do
            let listCall = M.fromPure1 (asListM bindings) (number sample + step)
                maybeCall = M.fromPure1 (asMaybeM bindings) (other sample - step)
            if even (variant sample + step) then do
                i <- listCall
                j <- maybeCall
                pure (i + j)
            else do
                j <- maybeCall
                i <- listCall
                pure (i + j)) [0 .. repeats sample - 1]
    , scalarSource "repeated-list-dictionary"
        (\sample -> sum [Source.asList (number sample + step)
            | step <- [0 .. repeats sample - 1]]) $ \sample -> do
        bindings <- initialize
        sum <$> mapM (M.fromPure1 (asListM bindings) . (number sample +))
            [0 .. repeats sample - 1]
    , scalarSource "repeated-maybe-dictionary"
        (\sample -> sum [Source.asMaybe (other sample - step)
            | step <- [0 .. repeats sample - 1]]) $ \sample -> do
        bindings <- initialize
        sum <$> mapM (M.fromPure1 (asMaybeM bindings) . (other sample -))
            [0 .. repeats sample - 1]
    ]
