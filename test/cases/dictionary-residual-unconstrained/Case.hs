module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, failure, scalarSource, snapshot)

type Views = M.PairT (M.PairT Int (L.ListT Bool)) (M.PairT Int (L.ListT Char))

generic :: M.Th Int -> M.MemoristM Views
generic = G.viewsM

call :: Bool -> Int -> M.MemoristM Views
call paid n = do
    count <- M.lazy (if paid then M.tick >> pure n else pure n)
    generic count

source :: Int -> Int
source n = let ((a,bs),(b,cs)) = Source.views n in a + b + length bs + length cs

first :: M.PairT Int a -> M.MemoristM Int
first (M.PairT n _) = M.force n

listLength :: L.ListT a -> M.MemoristM Int
listLength L.NilT = pure 0
listLength (L.ConsT _ tail) = (1 +) <$> (M.force tail >>= listLength)

second :: M.PairT a (L.ListT b) -> M.MemoristM Int
second (M.PairT _ list) = M.force list >>= listLength

pair :: M.PairT Int (L.ListT a) -> M.MemoristM Int
pair p = (+) <$> first p <*> second p

full :: Views -> M.MemoristM Int
full (M.PairT a b) = (+) <$> (M.force a >>= pair) <*> (M.force b >>= pair)

lists :: Views -> M.MemoristM Int
lists (M.PairT a b) = (+) <$> (M.force a >>= second) <*> (M.force b >>= second)

names :: M.Th (M.PairT a b) -> [M.ThName]
names root = case M.tval root of
    M.PairT a b -> [M.tname root, M.tname a, M.tname b]

annotations :: M.Th (M.PairT a b) -> [M.Annot]
annotations root = case M.tval root of
    M.PairT a b -> [M.tannot root, M.tannot a, M.tannot b]

aliases :: Views -> (Bool, Bool, Bool)
aliases (M.PairT a b) =
    (names a == names b, annotations a == annotations b,
     case names a of [r,n,l] -> r /= n && r /= l && n /= l; _ -> False)

freshNames :: Int -> M.MemoristM Bool
freshNames n = do
    M.PairT a b <- call False n
    M.PairT c d <- call False n
    pure (all (`notElem` (names c ++ names d)) (names a ++ names b))

cachedNames :: Int -> M.MemoristM Bool
cachedNames n = do
    saved <- M.lazy (call False n)
    M.PairT a b <- M.force saved
    M.PairT c d <- M.force saved
    pure (names a == names c && names b == names d)

probes :: [Probe]
probes =
    [ scalarSource "full/cost-2" (source . number) (\s -> call False (number s) >>= full)
    , scalarSource "first-only" (fst . fst . Source.views . number) (\s -> do
        M.PairT a _ <- call False (number s)
        M.force a >>= first)
    , scalarSource "second-only" (fst . snd . Source.views . number) (\s -> do
        M.PairT _ b <- call False (number s)
        M.force b >>= first)
    , scalarSource "lists/unused-bottom" (const 0) (\_ -> call False undefined >>= lists)
    , scalarSource "lists/paid-unused-bottom" (const 0) (\_ -> call True undefined >>= lists)
    , scalarSource "skip/cost-1" (const 0) (\s -> call False (number s) >> pure 0)
    , scalarSource "skip-all/bottom-count" (const 0)
        (\_ -> call True undefined >> pure 0)
    , scalarSource "cached/cost-2" (\s -> 2 * source (number s)) (\s -> do
        root <- M.lazy (call False (number s))
        sum <$> replicateM 2 (M.force root >>= full))
    , scalarSource "fresh/cost-4" (\s -> 2 * source (number s))
        (\s -> sum <$> replicateM 2 (call False (number s) >>= full))
    , scalarSource "paid/cost-3" (source . number) (\s -> call True (number s) >>= full)
    , failure "numeric-field/demanded-bottom" (\_ -> call False undefined >>= full)
    , snapshot "cross-type/root-numeric-list-names-and-annotations"
        (\s -> aliases (M.value (M.eval1 (call False (number s)))))
    , snapshot "fresh/root-numeric-list-separation"
        (\s -> M.value (M.eval1 (freshNames (number s))))
    , snapshot "cached/root-numeric-list-identities"
        (\s -> M.value (M.eval1 (cachedNames (number s))))
    , scalarSource "ordinary-work" (Source.work . number)
        (\s -> M.lazy (pure (number s)) >>= G.workM)
    ]
