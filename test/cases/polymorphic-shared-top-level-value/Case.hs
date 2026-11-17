module Case (probes) where

import Generated (intsM, boolsM)
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource, demanded)

probes :: [Probe]
probes =
    [ scalarSource "fixed-cross-type-calls"
        (\_ -> if Source.bools True then Source.ints (5 :: Int) else 0) $ \_ -> do
        flag <- M.fromPure1 boolsM True
        if flag then M.fromPure1 intsM (5 :: Int) else pure 0
    , scalarSource "cross-type-shared-calls"
        (\sample -> sum [if Source.bools (leftDemand sample /= even step)
            then Source.ints (number sample + step)
            else negate (Source.ints (number sample + step))
            | step <- [0 .. repeats sample - 1]]) $ \sample ->
        sum <$> mapM (\step -> do
            let intCall = M.fromPure1 intsM (number sample + step)
                boolCall = M.fromPure1 boolsM
                    (leftDemand sample /= even step)
            if even (variant sample + step) then do
                flag <- boolCall
                value <- intCall
                pure (if flag then value else negate value)
            else do
                value <- intCall
                flag <- boolCall
                pure (if flag then value else negate value))
            [0 .. repeats sample - 1]
    , scalarSource "conditional-bools-then-ints"
        (\sample -> sum [if Source.bools (rightDemand sample /= even step)
            then Source.ints (other sample - step) else 0
            | step <- [0 .. repeats sample - 1]]) $ \sample ->
        sum <$> mapM (\step -> do
            flag <- M.fromPure1 boolsM
                (rightDemand sample /= even step)
            if flag then M.fromPure1 intsM (other sample - step)
                else pure 0) [0 .. repeats sample - 1]
    , demanded "repeated-boolean-calls" (\_ -> id) (\_ -> M.fullDemand) $ \sample ->
        and <$> mapM (\step -> M.fromPure1 boolsM
            (leftDemand sample /= even step)) [0 .. repeats sample - 1]
    ]
