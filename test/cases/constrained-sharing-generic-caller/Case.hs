module Case (probes) where

import Generated (initialize, asIntM, asIntegerM)
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource, demanded)

probes :: [Probe]
probes =
    [ scalarSource "fixed-dictionary-sum"
        (\_ -> Source.asInt 0 + fromInteger (Source.asInteger 0)) $ \_ -> do
        bindings <- initialize
        i <- M.fromPure1 (asIntM bindings) (0 :: Int)
        j <- M.fromPure1 (asIntegerM bindings) (0 :: Integer)
        pure (i + fromInteger j)
    , demanded "fixed-dictionary-results" (\_ -> id) (\_ -> M.fullDemand) $ \_ -> do
        bindings <- initialize
        i <- M.fromPure1 (asIntM bindings) (0 :: Int)
        j <- M.fromPure1 (asIntegerM bindings) (0 :: Integer)
        pure (i, j)
    , scalarSource "interleaved-dictionaries"
        (\sample -> sum [Source.asInt (number sample + step)
            + fromInteger (Source.asInteger (toInteger (other sample - step)))
            | step <- [0 .. repeats sample - 1]]) $ \sample -> do
        bindings <- initialize
        sum <$> mapM (\step -> do
            let intCall = M.fromPure1 (asIntM bindings) (number sample + step)
                integerCall = M.fromPure1 (asIntegerM bindings)
                    (toInteger (other sample - step))
            if even (variant sample + step) then do
                i <- intCall
                j <- integerCall
                pure (i + fromInteger j)
            else do
                j <- integerCall
                i <- intCall
                pure (i + fromInteger j)) [0 .. repeats sample - 1]
    , scalarSource "int-dictionary-only"
        (\sample -> sum [Source.asInt (number sample + step)
            | step <- [0 .. repeats sample - 1]]) $ \sample -> do
        bindings <- initialize
        sum <$> mapM (M.fromPure1 (asIntM bindings) . (number sample +))
            [0 .. repeats sample - 1]
    , demanded "integer-dictionary-only" (\_ -> id) (\_ -> M.fullDemand) $ \sample -> do
        bindings <- initialize
        sum <$> mapM (M.fromPure1 (asIntegerM bindings) . toInteger . (other sample +))
            [0 .. repeats sample - 1]
    , scalarSource "selected-dictionaries"
        (\sample -> (if leftDemand sample then Source.asInt (number sample) else 0)
            + (if rightDemand sample
                then fromInteger (Source.asInteger (toInteger (other sample))) else 0)) $ \sample -> do
        bindings <- initialize
        i <- if leftDemand sample
            then M.fromPure1 (asIntM bindings) (number sample) else pure 0
        j <- if rightDemand sample
            then M.fromPure1 (asIntegerM bindings) (toInteger (other sample)) else pure 0
        pure (i + fromInteger j)
    ]
