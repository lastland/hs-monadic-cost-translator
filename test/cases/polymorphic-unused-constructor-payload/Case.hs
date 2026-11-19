module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, failure, scalarSource)

call :: Int -> M.MemoristM Int
call n = M.lazy (pure n) >>= G.viewsM

paid :: Int -> M.MemoristM Int
paid n = M.lazy (M.tick >> pure n) >>= G.viewsM

probes :: [Probe]
probes =
    [ scalarSource "unused-constructor/value" (Source.views . number)
        (call . number)
    , scalarSource "unused-constructor/min-bound" (const (Source.views minBound))
        (const (call minBound))
    , scalarSource "unused-constructor/max-bound" (const (Source.views maxBound))
        (const (call maxBound))
    , scalarSource "unused-constructor/paid-input" (Source.views . number)
        (paid . number)
    , scalarSource "unused-constructor/cached-demands"
        (\s -> repeats s * Source.views (number s)) $ \s -> do
            cached <- M.lazy (call (number s))
            sum <$> replicateM (repeats s) (M.force cached)
    , scalarSource "unused-constructor/fresh-invocations"
        (\s -> repeats s * Source.views (number s)) $ \s -> do
            count <- M.lazy (pure (number s))
            sum <$> replicateM (repeats s) (G.viewsM count)
    , failure "unused-constructor/demanded-input-bottom"
        (const (call (error "demanded input")))
    ]
