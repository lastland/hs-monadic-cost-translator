{-# LANGUAGE ExplicitForAll #-}
module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource, snapshot)

type Views = M.PairT Int Int

generic :: forall t. M.Th t -> M.Th Int -> M.MemoristM Views
generic = G.viewsM

call :: Int -> M.MemoristM Views
call n = do
    ignored <- M.lazy (error "unused generic argument")
    count <- M.lazy (pure n)
    generic (ignored :: M.Th ()) count

paid :: Int -> M.MemoristM Views
paid n = do
    ignored <- M.lazy (M.tick >> pure ())
    count <- M.lazy (M.tick >> pure n)
    generic ignored count

sourceSum :: Int -> Int
sourceSum n = let (a,b) = Source.views () n in a + b

sourceFirst :: Int -> Int
sourceFirst = fst . Source.views ()

fully :: Views -> M.MemoristM Int
fully (M.PairT a b) = (+) <$> M.force a <*> M.force b

first :: Views -> M.MemoristM Int
first (M.PairT a _) = M.force a

second :: Views -> M.MemoristM Int
second (M.PairT _ b) = M.force b

names :: Views -> (M.ThName, M.ThName)
names (M.PairT a b) = (M.tname a, M.tname b)

probes :: [Probe]
probes =
    [ scalarSource "vacuous/signed" (sourceSum . number)
        (\s -> call (number s) >>= fully)
    , scalarSource "vacuous/zero" (const (sourceSum 0))
        (const (call 0 >>= fully))
    , scalarSource "vacuous/min-bound" (const (sourceSum minBound))
        (const (call minBound >>= fully))
    , scalarSource "vacuous/max-bound" (const (sourceSum maxBound))
        (const (call maxBound >>= fully))
    , scalarSource "vacuous/first-only" (sourceFirst . number)
        (\s -> call (number s) >>= first)
    , scalarSource "vacuous/second-only" (sourceFirst . number)
        (\s -> call (number s) >>= second)
    , scalarSource "vacuous/skip-paid-count" (const 0)
        (\s -> paid (number s) >> pure 0)
    , scalarSource "vacuous/full-paid-count-once" (sourceSum . number)
        (\s -> paid (number s) >>= fully)
    , scalarSource "vacuous/cached-repeated-demands" (\s -> repeats s * sourceSum (number s)) $ \s -> do
        cached <- M.lazy (call (number s))
        sum <$> replicateM (repeats s) (M.force cached >>= fully)
    , scalarSource "vacuous/fresh-invocations" (\s -> repeats s * sourceSum (number s)) $ \s -> do
        ignored <- M.lazy (M.tick >> pure ())
        count <- M.lazy (pure (number s))
        sum <$> replicateM (repeats s) (generic ignored count >>= fully)
    , snapshot "vacuous/one-shared-root" $ \s -> M.value $ M.eval1 $ do
        (a,b) <- names <$> call (number s)
        pure (a == b)
    , snapshot "vacuous/fresh-root-identities" $ \s -> M.value $ M.eval1 $ do
        ignored <- M.lazy (M.tick >> pure ())
        count <- M.lazy (pure (number s))
        (a,b) <- names <$> generic ignored count
        (c,d) <- names <$> generic ignored count
        pure (a == b && c == d && a /= c)
    ]
