module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, failure, scalarSource)

generic :: Num t => M.Th t -> M.MemoristM ()
generic = G.viewsM

probes :: [Probe]
probes =
    [ scalarSource "unit/int-bottom-input" (const sourceInt)
        (const (call (undefined :: Int)))
    , scalarSource "unit/integer-bottom-input" (const sourceInteger)
        (const (call (undefined :: Integer)))
    , scalarSource "fresh-calls/same-unused-input" (\sample -> repeats sample * sourceInt) $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        sum <$> replicateM (repeats sample) (unitValue <$> generic input)
    , scalarSource "cached-call/repeated-unit-demands" (\sample -> repeats sample * sourceInt) $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        cached <- M.lazy (generic input)
        sum <$> replicateM (repeats sample) (unitValue <$> M.force cached)
    , scalarSource "fresh-calls/paid-input-remains-unused" (\sample -> repeats sample * sourceInt) $ \sample -> do
        input <- M.lazy (M.tick >> pure (undefined :: Int))
        sum <$> replicateM (repeats sample) (unitValue <$> generic input)
    , failure "control/demanded-input-really-bottom" $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.force input
        pure (if n == 0 then 0 else 1)
    ]

sourceInt, sourceInteger :: Int
sourceInt = case Source.views (undefined :: Int) of () -> 0
sourceInteger = case Source.views (undefined :: Integer) of () -> 0

unitValue :: () -> Int
unitValue result = case (result :: ()) of () -> 0

call :: Num t => t -> M.MemoristM Int
call input = do
    thunk <- M.lazy (pure input)
    unitValue <$> generic thunk
