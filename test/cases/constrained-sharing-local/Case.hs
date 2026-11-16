module Case (probes) where

import Generated (localM)
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource)

probes :: [Probe]
probes =
    [ scalarSource "fixed-local-dictionaries"
        (\_ -> Source.local (5 :: Int)) $ \_ -> M.fromPure1 localM (5 :: Int)
    , scalarSource "fresh-local-dictionaries-per-call"
        (\sample -> sum [Source.local (number sample + step)
            | step <- [0 .. repeats sample - 1]]) $ \sample ->
        sum <$> mapM (M.fromPure1 localM . (number sample +))
            [0 .. repeats sample - 1]
    ]
