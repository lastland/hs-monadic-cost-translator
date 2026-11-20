module Case (probes) where

import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Probe, Sample(..), distinct, failure, scalarSource)

-- Negative occurrence: neither the caller nor the generated API has a Functor.
newtype Consumer a = Consumer (a -> Bool)

ordinary :: M.Th Int -> M.MemoristM (G.HigherT f a)
ordinary = G.makeM

views :: M.Th Int -> M.MemoristM (M.PairT (G.HigherT f Int) (G.HigherT f Bool))
views = G.viewsM

type Views = M.PairT (G.HigherT Consumer Int) (G.HigherT Consumer Bool)

atConsumer :: M.Th Int -> M.MemoristM Views
atConsumer = views

paid :: Int -> M.MemoristM (M.Th Int)
paid n = M.lazy (M.tick >> pure n)

fixed :: G.HigherT f a -> M.MemoristM Int
fixed (G.HigherT n _) = M.force n

optional :: G.HigherT f a -> M.MemoristM Int
optional (G.HigherT _ t) = do
    tag <- M.force t
    pure (case tag of M.NothingT -> 0; M.JustT _ -> 1)

fields :: Views -> M.MemoristM Int
fields (M.PairT x y) = (+) <$> (M.force x >>= fixed) <*> (M.force y >>= fixed)

optionals :: Views -> M.MemoristM Int
optionals (M.PairT x y) = (+) <$> (M.force x >>= optional) <*> (M.force y >>= optional)

sourceFixed :: Source.Higher f a -> Int
sourceFixed (Source.Higher n _) = n

sourceBoth :: Sample -> Int
sourceBoth s = let (x,y) = Source.views (number s) in sourceFixed x + sourceFixed y

same :: M.Th a -> M.Th b -> Bool
same x y = M.tname x == M.tname y && M.tannot x == M.tannot y

probes :: [Probe]
probes =
    [ scalarSource "ordinary" (\s -> sourceFixed (Source.make (number s))) $ \s -> do
        input <- paid (number s)
        x <- ordinary input :: M.MemoristM (G.HigherT Consumer Bool)
        fixed x
    , scalarSource "ordinary-guarded-field" (const 0) $ \_ -> do
        x <- ordinary (error "unused thunk") :: M.MemoristM (G.HigherT Consumer Int)
        optional x
    , scalarSource "skip-root" (const 0) $ \_ ->
        atConsumer (error "unused thunk") >> pure 0
    , scalarSource "both-optional-tags-skip-input" (const 0) $ \_ ->
        atConsumer (error "unused thunk") >>= optionals
    , scalarSource "both-views" sourceBoth $ \s ->
        paid (number s) >>= atConsumer >>= fields
    , scalarSource "repeat-in-reverse-order" ((2 *) . sourceBoth) $ \s -> do
        pair@(M.PairT left right) <- paid (number s) >>= atConsumer
        first <- fields pair
        b <- M.force right >>= fixed
        a <- M.force left >>= fixed
        pure (first + b + a)
    , scalarSource "two-calls-same-input" ((2 *) . sourceBoth) $ \s -> do
        input <- paid (number s)
        (+) <$> (atConsumer input >>= fields) <*> (atConsumer input >>= fields)
    , scalarSource "two-calls-fresh-input" ((2 *) . sourceBoth) $ \s -> do
        let call = paid (number s) >>= atConsumer >>= fields
        (+) <$> call <*> call
    , scalarSource "cross-type-aliases-and-fresh-allocations" (const 1) $ \s -> do
        x <- paid (number s)
        y <- paid (number s)
        M.PairT l r <- atConsumer x
        M.PairT again _ <- atConsumer x
        M.PairT fresh _ <- atConsumer y
        G.HigherT a am <- M.force l
        G.HigherT b bm <- M.force r
        G.HigherT c cm <- M.force again
        G.HigherT d dm <- M.force fresh
        pure (fromEnum (same l r && same am bm && all (same x) [a,b,c] && same y d &&
            distinct [M.tname l, M.tname again, M.tname fresh, M.tname am, M.tname cm,
                      M.tname dm, M.tname x, M.tname y]))
    , failure "force-failing-input" $ \_ ->
        M.lazy (error "demanded argument") >>= atConsumer >>= fields
    ]
