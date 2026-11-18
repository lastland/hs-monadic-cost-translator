module Case (probes) where

import Control.Monad (replicateM)
import Data.List (nub)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, failure, scalarSource, snapshot)

type Row d = M.PairT Int (M.PairT Int (M.PairT Int d))
type Views = M.PairT (M.PairT (Row Int) (Row Int)) (M.PairT (Row Integer) (Row Integer))

generic :: Num t => M.Th t -> M.Th Int -> M.MemoristM Views
generic = G.viewsM

genericDistinct :: Num t => M.Th t -> M.MemoristM (M.PairT (Row Int) (Row Int))
genericDistinct = G.distinctM

probes :: [Probe]
probes =
    [ scalarSource "full/outer-int-bottom" (sourceFull . positive) $ \s ->
        call (undefined :: Int) (positive s) >>= forceViews True True
    , scalarSource "full/outer-integer-bottom" (sourceFull . positive) $ \s ->
        call (undefined :: Integer) (positive s) >>= forceViews True True
    , scalarSource "common-first-three/last-bottom-skipped" (const (sourceCommon 0)) $ \_ ->
        call (undefined :: Int) 0 >>= forceViews True False
    , scalarSource "last-only/left-bottom-skipped" (const (sourceLast (-1))) $ \_ ->
        call (undefined :: Int) (-1) >>= forceViews False True
    , demanded "skip-all/bottom-count" (\_ _ -> ()) (\_ _ -> M.PairD M.Skip M.Skip)
        (const (call (undefined :: Int) (undefined :: Int)))
    , demanded "roots-only/bottom-count" (\_ _ -> ()) (\_ _ -> rootDemand)
        (const (call (undefined :: Int) (undefined :: Int)))
    , demanded "demanded-full" (const pureSum) (const M.fullDemand)
        (call (undefined :: Int) . positive)
    , failure "left/demanded-negative" (\_ -> call (undefined :: Int) (-1) >>= forceViews True False)
    , failure "last/demanded-zero" (\_ -> call (undefined :: Int) 0 >>= forceViews False True)
    , failure "leaf/demanded-bottom-count" (\_ -> call (undefined :: Int) (undefined :: Int) >>= forceViews True False)
    , scalarSource "fresh-invocations" (\s -> safeRepeats s * sourceFull (positive s)) $ \s -> do
        input <- M.lazy (pure (undefined :: Int))
        count <- M.lazy (pure (positive s))
        sum <$> replicateM (safeRepeats s) (generic input count >>= forceViews True True)
    , scalarSource "cached-call/repeated-demands" (\s -> safeRepeats s * sourceFull (positive s)) $ \s -> do
        cached <- M.lazy (call (undefined :: Int) (positive s))
        sum <$> replicateM (safeRepeats s) (M.force cached >>= forceViews True True)
    , scalarSource "paid-captured-count/fresh-calls" (\s -> safeRepeats s * sourceFull (positive s)) $ \s -> do
        input <- M.lazy (pure (undefined :: Int))
        count <- M.lazy (M.tick >> pure (positive s))
        sum <$> replicateM (safeRepeats s) (generic input count >>= forceViews True True)
    , scalarSource "distinct-source-bindings" (const (sourceDistinct ())) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        genericDistinct input >>= forceCopies True True
    , snapshot "identity/all-nested-fields-and-complete-dictionaries" $ \s -> M.value $ M.eval1 $ do
        groups <- call (undefined :: Int) (positive s) >>= viewNames
        pure (validGroups groups)
    , snapshot "identity/fresh-locals-caller-captures-reused" $ \s -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        count <- M.lazy (pure (positive s))
        first <- generic input count >>= viewNames
        second <- generic input count >>= viewNames
        pure (validGroups first && validGroups second && disjoint (concat first) (concat second)
            && disjoint (concat first ++ concat second) [M.tname input,M.tname count])
    , snapshot "identity/distinct-source-bindings" $ \_ -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        M.PairT first second <- genericDistinct input
        a <- rowNames first
        b <- rowNames second
        pure (disjoint a b)
    ]

positive, safeRepeats :: Sample -> Int
positive = (1 +) . abs . number
safeRepeats = max 0 . repeats

call :: Num t => t -> Int -> M.MemoristM Views
call input count = do
    x <- M.lazy (pure input)
    n <- M.lazy (pure count)
    generic x n

sourceFull, sourceCommon, sourceLast :: Int -> Int
sourceFull n = sourceSum True True (Source.views (undefined :: Int) n)
sourceCommon n = sourceSum True False (Source.views (undefined :: Int) n)
sourceLast n = sourceSum False True (Source.views (undefined :: Int) n)

sourceSum :: Bool -> Bool -> (((Int,(Int,(Int,Int))),(Int,(Int,(Int,Int)))),((Int,(Int,(Int,Integer))),(Int,(Int,(Int,Integer))))) -> Int
sourceSum common lastField ((a,b),(c,d)) = selected a + selected b + selected c + selected d
  where
    selected :: Integral d => (Int,(Int,(Int,d))) -> Int
    selected (w,(x,(y,z))) = (if common then w+x+y else 0) + (if lastField then fromIntegral z else 0)

sourceDistinct :: () -> Int
sourceDistinct _ = let (a,b) = Source.distinct (undefined :: Int) in selected a + selected b
  where selected (w,(x,(y,z))) = w+x+y+z

forceRow :: Integral d => Bool -> Bool -> Row d -> M.MemoristM Int
forceRow common lastField (M.PairT a rest) = do
    w <- if common then M.force a else pure 0
    M.PairT b inner <- M.force rest
    x <- if common then M.force b else pure 0
    M.PairT c d <- M.force inner
    y <- if common then M.force c else pure 0
    z <- if lastField then fromIntegral <$> M.force d else pure 0
    pure (w+x+y+z)

forceCopies :: Integral d => Bool -> Bool -> M.PairT (Row d) (Row d) -> M.MemoristM Int
forceCopies common lastField (M.PairT first second) = do
    a <- M.force first >>= forceRow common lastField
    b <- M.force second >>= forceRow common lastField
    pure (a+b)

forceViews :: Bool -> Bool -> Views -> M.MemoristM Int
forceViews common lastField (M.PairT ints integers) = do
    a <- M.force ints >>= forceCopies common lastField
    b <- M.force integers >>= forceCopies common lastField
    pure (a+b)

pureSum :: Views -> Int
pureSum (M.PairT ints integers) = copies (M.tval ints) + copies (M.tval integers)
  where
    copies :: Integral d => M.PairT (Row d) (Row d) -> Int
    copies (M.PairT a b) = row (M.tval a) + row (M.tval b)
    row :: Integral d => Row d -> Int
    row (M.PairT a rest) =
        let M.PairT b inner = M.tval rest
            M.PairT c d = M.tval inner
        in M.tval a + M.tval b + M.tval c + fromIntegral (M.tval d)

rootDemand :: M.PairD (M.PairD (M.PairD Int (M.PairD Int (M.PairD Int Int))) (M.PairD Int (M.PairD Int (M.PairD Int Int)))) (M.PairD (M.PairD Int (M.PairD Int (M.PairD Int Integer))) (M.PairD Int (M.PairD Int (M.PairD Int Integer))))
rootDemand = M.PairD (M.Need (M.PairD root root)) (M.Need (M.PairD root root))
  where root = M.Need (M.PairD M.Skip M.Skip)

rowNames :: M.Th (M.PairT a (M.PairT b (M.PairT c d))) -> M.MemoristM [M.ThName]
rowNames root = do
    M.PairT a rest <- M.force root
    M.PairT b inner <- M.force rest
    M.PairT c d <- M.force inner
    pure [M.tname root,M.tname a,M.tname rest,M.tname b,M.tname inner,M.tname c,M.tname d]

viewNames :: Views -> M.MemoristM [[M.ThName]]
viewNames (M.PairT ints integers) = do
    M.PairT a b <- M.force ints
    M.PairT c d <- M.force integers
    sequence [rowNames a,rowNames b,rowNames c,rowNames d]

disjoint :: Eq a => [a] -> [a] -> Bool
disjoint a b = all (`notElem` b) a

validGroups :: [[M.ThName]] -> Bool
validGroups [a,b,c,d] = length a == 7 && length c == 7 && nub a == a && nub c == c
    && a == b && c == d && disjoint a c
validGroups _ = False
