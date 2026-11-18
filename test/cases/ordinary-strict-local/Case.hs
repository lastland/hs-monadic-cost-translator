module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource)

generic :: Num t => M.Th t -> M.MemoristM ()
generic = G.viewsM

unit :: () -> Int
unit () = 0

call :: Num t => t -> M.MemoristM Int
call n = do
    input <- M.lazy (pure n)
    unit <$> generic input

probes :: [Probe]
probes =
    [ scalarSource "finite/int" (unit . Source.views . number) (call . number)
    , scalarSource "finite/integer" (unit . Source.views . toInteger . number) (call . toInteger . number)
    , scalarSource "fresh/paid-input" (\s -> repeats s * unit (Source.views (number s))) $ \s -> do
        input <- M.lazy (M.tick >> pure (number s))
        sum <$> replicateM (repeats s) (unit <$> generic input)
    , scalarSource "cached/paid-input" (\s -> repeats s * unit (Source.views (number s))) $ \s -> do
        input <- M.lazy (M.tick >> pure (number s))
        result <- M.lazy (generic input)
        sum <$> replicateM (repeats s) (unit <$> M.force result)
    , scalarSource "bottom/int" (const (unit (Source.views (undefined :: Int))))
        (const (call (undefined :: Int)))
    , scalarSource "bottom/integer" (const (unit (Source.views (undefined :: Integer))))
        (const (call (undefined :: Integer)))
    ]
