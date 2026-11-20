module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, failure, scalarSource, snapshot)

generic :: Integral a => M.Th a -> M.Th t -> M.Th Int -> M.MemoristM (M.PairT Int Int)
generic = G.viewsM

call :: Int -> M.MemoristM (M.PairT Int Int)
call n = do
    seed <- M.lazy (M.tick >> pure (undefined :: Integer))
    extra <- M.lazy (M.tick >> pure (undefined :: Bool))
    count <- M.lazy (M.tick >> pure n)
    generic seed extra count

total :: M.PairT Int Int -> M.MemoristM Int
total (M.PairT x y) = (+) <$> M.force x <*> M.force y

source :: Int -> Int
source n = let (x,y) = Source.views (undefined :: Integer) (undefined :: Bool) n in x+y

probes :: [Probe]
probes =
    [ scalarSource "full/one-paid-field" (source . number) (\s -> call (number s) >>= total)
    , scalarSource "entry/paid-fields-unused" (const 0) (\s -> call (number s) >> pure 0)
    , scalarSource "partial/left" number (\s -> call (number s) >>= \(M.PairT x _) -> M.force x)
    , scalarSource "partial/right" number (\s -> call (number s) >>= \(M.PairT _ y) -> M.force y)
    , failure "branch/needs-count" (\_ -> call undefined >> pure 0)
    , scalarSource "cached/once" (\s -> repeats s * source (number s)) (\s -> do
        saved <- M.lazy (call (number s))
        sum <$> replicateM (repeats s) (M.force saved >>= total))
    , scalarSource "fresh/per-invocation" (\s -> repeats s * source (number s))
        (\s -> sum <$> replicateM (repeats s) (call (number s) >>= total))
    , snapshot "identity/field-and-annotation" (\s -> M.value $ M.eval1 $ do
        M.PairT x y <- call (number s)
        pure (M.tname x == M.tname y, M.tannot x == M.tannot y))
    , snapshot "identity/fresh-producer-with-same-input-thunks" (\s -> M.value $ M.eval1 $ do
        seed <- M.lazy (pure (undefined :: Integer))
        extra <- M.lazy (pure (undefined :: Bool))
        count <- M.lazy (pure (number s))
        M.PairT x y <- generic seed extra count
        M.PairT z w <- generic seed extra count
        pure (M.tname x == M.tname y && M.tname z == M.tname w && M.tname x /= M.tname z))
    ]
