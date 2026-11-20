module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, failure, scalarSource, snapshot)

type Copies k = M.PairT (M.PairT k (L.ListT Bool)) (M.PairT k (L.ListT Char))
type Views k = M.PairT (Copies k) (Copies Integer)

generic :: Integral k => M.Th k -> M.Th Int -> M.MemoristM (Views k)
generic = G.viewsM

call :: Integral k => k -> Int -> M.MemoristM (Views k)
call k n = do
    arg <- M.lazy (M.tick >> pure k)
    count <- M.lazy (pure n)
    generic arg count

source :: Integral k => k -> Int -> Int
source k n = let (left,right) = Source.views k n
                 total ((x,_),(y,_)) = fromIntegral (x+y)
             in total left + total right

numbersIn :: Integral k => Copies k -> M.MemoristM Int
numbersIn (M.PairT a b) = (+) <$> (M.force a >>= numberIn) <*> (M.force b >>= numberIn)
  where numberIn (M.PairT n _) = fromIntegral <$> M.force n

full :: Integral k => Views k -> M.MemoristM Int
full (M.PairT a b) = (+) <$> (M.force a >>= numbersIn) <*> (M.force b >>= numbersIn)

lists :: Copies k -> M.MemoristM Int
lists (M.PairT a b) = (+) <$> (M.force a >>= list) <*> (M.force b >>= list)
  where list (M.PairT _ xs) = M.force xs >>= \v -> pure (case v of L.NilT -> 0; L.ConsT _ _ -> 1)

nodes :: M.Th (M.PairT a b) -> [(M.ThName, M.Annot)]
nodes root = case M.tval root of
    M.PairT n xs -> [node root, node n, node xs]
  where node t = (M.tname t, M.tannot t)

roots :: Views k -> (Bool, [M.ThName])
roots (M.PairT a b) =
    let group t = case M.tval t of
            M.PairT x y -> (nodes x == nodes y, map fst (nodes x))
        (aok, an) = group a
        (bok, bn) = group b
        different [x,y,z] = x /= y && y /= z && x /= z
        different _ = False
    in (aok && bok && different an && different bn && all (`notElem` bn) an, an ++ bn)

probes :: [Probe]
probes =
    [ scalarSource "full/distinct-key-types" (source (0 :: Int) . number)
        (\s -> call (0 :: Int) (number s) >>= full)
    , scalarSource "full/same-key-type-separate-helper-calls" (source (0 :: Integer) . number)
        (\s -> call (0 :: Integer) (number s) >>= full)
    , scalarSource "one-helper-only" (\s -> source (0 :: Int) (number s) `quot` 2)
        (\s -> call (0 :: Int) (number s) >>= \(M.PairT a _) -> M.force a >>= numbersIn)
    , scalarSource "skip/paid-unused-seed" (const 0) (\_ -> call (undefined :: Int) undefined >> pure 0)
    , scalarSource "lists/bottom-undemanded" (const 0) (\_ -> do
        M.PairT a b <- call (undefined :: Int) undefined
        (+) <$> (M.force a >>= lists) <*> (M.force b >>= lists))
    , failure "number/bottom-demanded" (\_ -> call (0 :: Int) undefined >>= full)
    , scalarSource "cached/once" (\s -> repeats s * source (0 :: Int) (number s)) (\s -> do
        saved <- M.lazy (call (0 :: Int) (number s))
        sum <$> replicateM (repeats s) (M.force saved >>= full))
    , scalarSource "fresh/per-invocation" (\s -> repeats s * source (0 :: Int) (number s))
        (\s -> sum <$> replicateM (repeats s) (call (0 :: Int) (number s) >>= full))
    , snapshot "identity/names-and-annotations/same-key" (\s ->
        fst (roots (M.value (M.eval1 (call (0 :: Integer) (number s))))))
    , snapshot "identity/separate-invocations" (\s -> M.value $ M.eval1 $ do
        x <- roots <$> call (0 :: Int) (number s)
        y <- roots <$> call (0 :: Int) (number s)
        pure (fst x && fst y && all (`notElem` snd y) (snd x)))
    ]
