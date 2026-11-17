module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, failure, scalarSource, snapshot)

type Views = M.PairT (M.PairT (M.PairT Int Int) (M.PairT Int Int)) (M.PairT (M.PairT Int Integer) (M.PairT Int Integer))

probes :: [Probe]
probes =
    [ scalarSource "full/outer-int-bottom" (sourceFull . positive) $ \s ->
        call (undefined :: Int) (positive s) >>= forceViews True True
    , scalarSource "full/outer-integer-bottom" (sourceFull . positive) $ \s ->
        call (undefined :: Integer) (positive s) >>= forceViews True True
    , scalarSource "first-field/second-dictionary-still-distinct" (sourceLeft . nonnegative) $ \s ->
        call (undefined :: Int) (nonnegative s) >>= forceViews True False
    , scalarSource "right-field/left-bottom-skipped" (const (sourceRight (-1))) $ \_ ->
        call (undefined :: Int) (-1) >>= forceViews False True
    , demanded "skip-all/bottom-count" (\_ _ -> ()) (\_ _ -> M.PairD M.Skip M.Skip)
        (const (call (undefined :: Int) (undefined :: Int)))
    , demanded "roots-only/bottom-count" (\_ _ -> ()) (\_ _ -> rootDemand)
        (const (call (undefined :: Int) (undefined :: Int)))
    , demanded "demanded-full" (const pureSum) (const M.fullDemand)
        (call (undefined :: Int) . positive)
    , failure "left/demanded-negative" (\_ -> call (undefined :: Int) (-1) >>= forceViews True False)
    , failure "right/demanded-zero" (\_ -> call (undefined :: Int) 0 >>= forceViews False True)
    , failure "leaf/demanded-bottom-count" (\_ -> call (undefined :: Int) (undefined :: Int) >>= forceViews True False)
    , scalarSource "fresh-invocations" (\s -> repeats s * sourceFull (positive s)) $ \s -> do
        input <- M.lazy (pure (undefined :: Int))
        count <- M.lazy (pure (positive s))
        sum <$> replicateM (repeats s) (generic input count >>= forceViews True True)
    , scalarSource "cached-call/repeated-demands" (\s -> repeats s * sourceFull (positive s)) $ \s -> do
        cached <- M.lazy (call (undefined :: Int) (positive s))
        sum <$> replicateM (repeats s) (M.force cached >>= forceViews True True)
    , scalarSource "paid-captured-count/fresh-calls" (\s -> repeats s * sourceFull (positive s)) $ \s -> do
        input <- M.lazy (pure (undefined :: Int))
        count <- M.lazy (M.tick >> pure (positive s))
        sum <$> replicateM (repeats s) (generic input count >>= forceViews True True)
    , scalarSource "distinct-source-bindings" (const (sourceDistinct ())) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        genericDistinct input >>= forceCopies True True
    , snapshot "identity/aliases-and-both-dictionaries" $ \s -> M.value $ M.eval1 $ do
        groups <- call (undefined :: Int) (positive s) >>= viewNames
        pure (validGroups groups)
    , snapshot "identity/fresh-invocations" $ \s -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        count <- M.lazy (pure (positive s))
        first <- generic input count >>= viewNames
        second <- generic input count >>= viewNames
        pure (validGroups first && validGroups second && disjoint (concat first) (concat second))
    , snapshot "identity/distinct-source-bindings" $ \_ -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        M.PairT first second <- genericDistinct input
        a <- pairNames first
        b <- pairNames second
        pure (disjoint a b)
    ]

generic :: Num t => M.Th t -> M.Th Int -> M.MemoristM (M.PairT (M.PairT (M.PairT Int Int) (M.PairT Int Int)) (M.PairT (M.PairT Int Integer) (M.PairT Int Integer)))
generic = G.viewsM

genericDistinct :: Num t => M.Th t -> M.MemoristM (M.PairT (M.PairT Int Int) (M.PairT Int Int))
genericDistinct = G.distinctM

positive :: Sample -> Int
positive = (1 +) . abs . number

nonnegative :: Sample -> Int
nonnegative = abs . number

call :: Num t => t -> Int -> M.MemoristM Views
call input count = do
    x <- M.lazy (pure input)
    n <- M.lazy (pure count)
    generic x n

sourceFull, sourceLeft, sourceRight :: Int -> Int
sourceFull n = sourceSum True True (Source.views (undefined :: Int) n)
sourceLeft n = sourceSum True False (Source.views (undefined :: Int) n)
sourceRight n = sourceSum False True (Source.views (undefined :: Int) n)

sourceSum :: Bool -> Bool -> (((Int, Int), (Int, Int)), ((Int, Integer), (Int, Integer))) -> Int
sourceSum wantLeft wantRight ((a,b),(c,d)) = selected a + selected b + selected c + selected d
  where
    selected :: Integral b => (Int,b) -> Int
    selected (x,y) = (if wantLeft then x else 0) + (if wantRight then fromIntegral y else 0)

sourceDistinct :: () -> Int
sourceDistinct _ = let ((a,b),(c,d)) = Source.distinct (undefined :: Int) in a+b+c+d

forcePair :: Integral b => Bool -> Bool -> M.PairT Int b -> M.MemoristM Int
forcePair wantLeft wantRight (M.PairT a b) = do
    x <- if wantLeft then M.force a else pure 0
    y <- if wantRight then fromIntegral <$> M.force b else pure 0
    pure (x+y)

forceCopies :: Integral b => Bool -> Bool -> M.PairT (M.PairT Int b) (M.PairT Int b) -> M.MemoristM Int
forceCopies l r (M.PairT first second) = do
    a <- M.force first >>= forcePair l r
    b <- M.force second >>= forcePair l r
    pure (a+b)

forceViews :: Bool -> Bool -> Views -> M.MemoristM Int
forceViews l r (M.PairT ints integers) = do
    a <- M.force ints >>= forceCopies l r
    b <- M.force integers >>= forceCopies l r
    pure (a+b)

pureSum :: Views -> Int
pureSum (M.PairT ints integers) = copies (M.tval ints) + copies (M.tval integers)
  where
    copies :: Integral b => M.PairT (M.PairT Int b) (M.PairT Int b) -> Int
    copies (M.PairT a b) = pairSum (M.tval a) + pairSum (M.tval b)
    pairSum :: Integral b => M.PairT Int b -> Int
    pairSum (M.PairT a b) = M.tval a + fromIntegral (M.tval b)

rootDemand :: M.PairD (M.PairD (M.PairD Int Int) (M.PairD Int Int)) (M.PairD (M.PairD Int Integer) (M.PairD Int Integer))
rootDemand = M.PairD (M.Need (M.PairD root root)) (M.Need (M.PairD root root))
  where root = M.Need (M.PairD M.Skip M.Skip)

pairNames :: M.Th (M.PairT a b) -> M.MemoristM [M.ThName]
pairNames root = do
    M.PairT a b <- M.force root
    pure [M.tname root, M.tname a, M.tname b]

viewNames :: Views -> M.MemoristM [[M.ThName]]
viewNames (M.PairT ints integers) = do
    M.PairT a b <- M.force ints
    M.PairT c d <- M.force integers
    sequence [pairNames a, pairNames b, pairNames c, pairNames d]

disjoint :: Eq a => [a] -> [a] -> Bool
disjoint a b = all (`notElem` b) a

validGroups :: [[M.ThName]] -> Bool
validGroups [a,b,c,d] = a == b && c == d && disjoint a c
validGroups _ = False
