module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, scalarSource, snapshot)

type Views a = M.PairT (M.PairT a Int) (M.PairT a Int)

probes :: [Probe]
probes =
    [ scalarSource "full/double" (sourceSum True True . number) $ \s ->
        call (number s) >>= forceCopies True True
    , scalarSource "left/right-skipped" (sourceSum True False . number) $ \s ->
        call (number s) >>= forceCopies True False
    , scalarSource "right/left-skipped" (sourceSum False True . number) $ \s ->
        call (number s) >>= forceCopies False True
    , demanded "selected-leaves" projectSelected demandSelected (call . number)
    , demanded "skip-all/paid-count-skipped" (\_ _ -> ()) (\_ _ -> M.PairD M.Skip M.Skip)
        (paidCount . number)
    , demanded "roots-only/paid-count-skipped" (\_ _ -> ()) (\_ _ -> rootDemand)
        (paidCount . number)
    , scalarSource "full/paid-count" (sourceSum True True . number) $ \s ->
        paidCount (number s) >>= forceCopies True True
    , scalarSource "full/ignored-paid-seed" (sourceSum True True . number) $ \s -> do
        seed <- M.lazy (M.tick >> pure (42 :: Double))
        count <- M.lazy (pure (number s))
        generic seed count >>= forceCopies True True
    , scalarSource "four-fresh" (\s -> 4 * sourceSum True True (number s)) $ \s ->
        sum <$> replicateM 4 (call (number s) >>= forceCopies True True)
    , scalarSource "four-cached" (\s -> 4 * sourceSum True True (number s)) $ \s -> do
        cached <- M.lazy (call (number s))
        sum <$> replicateM 4 (M.force cached >>= forceCopies True True)
    , scalarSource "repeated/fresh" repeatedSource $ \s -> do
        seed <- M.lazy (pure (42 :: Double))
        count <- M.lazy (pure (number s))
        sum <$> replicateM (repeats s) (generic seed count >>= forceCopies True True)
    , scalarSource "repeated/cached" repeatedSource $ \s -> do
        cached <- M.lazy (call (number s))
        sum <$> replicateM (repeats s) (M.force cached >>= forceCopies True True)
    , snapshot "identity/distinct-roots-shared-leaves" $ \s -> M.value $ M.eval1 $ do
        groups <- call (number s) >>= names
        pure (valid groups)
    , snapshot "identity/fresh-invocations" $ \s -> M.value $ M.eval1 $ do
        seed <- M.lazy (pure (42 :: Double))
        count <- M.lazy (pure (number s))
        first <- generic seed count >>= names
        second <- generic seed count >>= names
        pure (valid first && valid second && all (`notElem` concat second) (concat first))
    ]

-- GHC infers (Fractional a, Ord p, Num p) for views, with no source signature.
-- Choosing Int for p must not add Typeable a to this ordinary generic caller.
generic :: Fractional a => M.Th a -> M.Th Int -> M.MemoristM (Views a)
generic = G.viewsM

call :: Int -> M.MemoristM (Views Double)
call n = do
    seed <- M.lazy (pure (42 :: Double))
    count <- M.lazy (pure n)
    generic seed count

paidCount :: Int -> M.MemoristM (Views Double)
paidCount n = do
    seed <- M.lazy (pure (42 :: Double))
    count <- M.lazy (M.tick >> pure n)
    generic seed count

repeatedSource :: Sample -> Int
repeatedSource s = repeats s * sourceSum True True (number s)

sourceSum :: Bool -> Bool -> Int -> Int
sourceSum l r n = selected first + selected second
  where
    (first, second) = Source.views (42 :: Double) n
    selected (a,b) = (if l then round a else 0) + (if r then b else 0)

forceCopies :: Bool -> Bool -> Views Double -> M.MemoristM Int
forceCopies l r (M.PairT first second) = do
    a <- M.force first >>= selected
    b <- M.force second >>= selected
    pure (a+b)
  where
    selected (M.PairT a b) = do
        x <- if l then round <$> M.force a else pure 0
        y <- if r then M.force b else pure 0
        pure (x+y)

rootDemand :: M.PairD (M.PairD Double Int) (M.PairD Double Int)
rootDemand = M.PairD root root
  where root = M.Need (M.PairD M.Skip M.Skip)

projectSelected :: Sample -> Views Double -> ((Maybe Double, Maybe Int), (Maybe Double, Maybe Int))
projectSelected s (M.PairT first second) = (selected (M.tval first), selected (M.tval second))
  where
    selected (M.PairT a b) =
        (if leftDemand s then Just (M.tval a) else Nothing,
         if rightDemand s then Just (M.tval b) else Nothing)

demandSelected :: Sample -> Views Double -> M.PairD (M.PairD Double Int) (M.PairD Double Int)
demandSelected s (M.PairT first second) = M.PairD (selected first) (selected second)
  where
    selected root = let M.PairT a b = M.tval root in M.Need (M.PairD
        (if leftDemand s then M.fullDemand a else M.Skip)
        (if rightDemand s then M.fullDemand b else M.Skip))

names :: Views a -> M.MemoristM [[M.ThName]]
names (M.PairT first second) = sequence [group first, group second]
  where
    group root = do
        M.PairT a b <- M.force root
        pure [M.tname root, M.tname a, M.tname b]

valid :: [[M.ThName]] -> Bool
valid [[first,a,b],[second,c,d]] =
    first /= second && a == c && b == d && distinct [first,second,a,b]
  where
    distinct [] = True
    distinct (x:xs) = x `notElem` xs && distinct xs
valid _ = False
