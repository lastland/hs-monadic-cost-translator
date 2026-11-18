module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, scalarSource, snapshot)

type Copies a b = M.PairT (M.PairT a b) (M.PairT a b)
type Views a = M.PairT (Copies a Int) (Copies a Integer)
type CopiesD a b = M.PairD (M.PairD a b) (M.PairD a b)
type ViewsD a = M.PairD (CopiesD a Int) (CopiesD a Integer)

probes :: [Probe]
probes =
    [ scalarSource "full/int" (sourceSum True True . number) $ \s ->
        call (0 :: Int) (number s) >>= forceViews True True
    , scalarSource "full/integer" (sourceSum True True . number) $ \s ->
        call (0 :: Integer) (number s) >>= forceViews True True
    , scalarSource "left/right-skipped" (const (sourceSum True False 0)) $ \_ ->
        callInts 0 >>= forceViews True False
    , scalarSource "right/left-skipped" (const (sourceSum False True (-1))) $ \_ ->
        callInts (-1) >>= forceViews False True
    , demanded "skip-all/paid-count-skipped" (\_ _ -> ()) (\_ _ -> M.PairD M.Skip M.Skip)
        (paidCount . number)
    , demanded "roots-only/paid-count-skipped" (\_ _ -> ()) (\_ _ -> rootDemand)
        (paidCount . number)
    , demanded "selected-leaves" projectSelected demandSelected (callInts . number)
    , scalarSource "repeated/int/uncached" repeatedSource $ \s -> do
        a <- M.lazy (pure (0 :: Int))
        count <- M.lazy (pure (number s))
        sum <$> replicateM (repeats s) (generic a count >>= forceViews True True)
    , scalarSource "repeated/int/cached" repeatedSource $ \s -> do
        cached <- M.lazy (callInts (number s))
        sum <$> replicateM (repeats s) (M.force cached >>= forceViews True True)
    , scalarSource "repeated/integer/uncached" repeatedSource $ \s -> do
        a <- M.lazy (pure (0 :: Integer))
        count <- M.lazy (pure (number s))
        sum <$> replicateM (repeats s) (generic a count >>= forceViews True True)
    , scalarSource "repeated/integer/cached" repeatedSource $ \s -> do
        cached <- M.lazy (call (0 :: Integer) (number s))
        sum <$> replicateM (repeats s) (M.force cached >>= forceViews True True)
    , snapshot "identity/aliases-and-disjoint-partitions" $ \s -> M.value $ M.eval1 $ do
        groups <- callInts (number s) >>= names
        pure (valid groups)
    , snapshot "identity/fresh-invocations" $ \s -> M.value $ M.eval1 $ do
        a <- M.lazy (pure (0 :: Int))
        count <- M.lazy (pure (number s))
        first <- generic a count >>= names
        second <- generic a count >>= names
        pure (valid first && valid second && all (`notElem` concat second) (concat first))
    ]

-- The source context supplies Num a by superclass evidence. Int and Integer
-- have closed instances and cannot unify, whatever the caller chooses for a.
generic :: Integral a => M.Th a -> M.Th Int -> M.MemoristM (Views a)
generic = G.viewsM

call :: Integral a => a -> Int -> M.MemoristM (Views a)
call a count = do
    x <- M.lazy (pure a)
    n <- M.lazy (pure count)
    generic x n

callInts :: Int -> M.MemoristM (Views Int)
callInts = call (0 :: Int)

paidCount :: Int -> M.MemoristM (Views Int)
paidCount n = do
    a <- M.lazy (pure (0 :: Int))
    count <- M.lazy (M.tick >> pure n)
    generic a count

repeatedSource :: Sample -> Int
repeatedSource s = repeats s * sourceSum True True (number s)

sourceSum :: Bool -> Bool -> Int -> Int
sourceSum l r n = group ints + group integers
  where
    (ints, integers) = Source.views (0 :: Int) n
    group (first, second) = selected first + selected second
    selected (a,b) = (if l then fromIntegral a else 0) + (if r then fromIntegral b else 0)

forceViews :: Integral a => Bool -> Bool -> Views a -> M.MemoristM Int
forceViews l r (M.PairT ints integers) = do
    a <- M.force ints >>= copies
    b <- M.force integers >>= copies
    pure (a+b)
  where
    copies (M.PairT first second) = (+) <$> (M.force first >>= selected) <*> (M.force second >>= selected)
    selected (M.PairT a b) = do
        x <- if l then fromIntegral <$> M.force a else pure 0
        y <- if r then fromIntegral <$> M.force b else pure 0
        pure (x+y)

rootDemand :: ViewsD Int
rootDemand = M.PairD group group
  where
    group = M.Need (M.PairD root root)
    root = M.Need (M.PairD M.Skip M.Skip)

projectSelected :: Sample -> Views Int -> (((Maybe Int, Maybe Int), (Maybe Int, Maybe Int)), ((Maybe Int, Maybe Int), (Maybe Int, Maybe Int)))
projectSelected s (M.PairT ints integers) = (group (M.tval ints), group (M.tval integers))
  where
    group (M.PairT first second) = (selected (M.tval first), selected (M.tval second))
    selected (M.PairT a b) =
        (if leftDemand s then Just (fromIntegral (M.tval a)) else Nothing,
         if rightDemand s then Just (fromIntegral (M.tval b)) else Nothing)

demandSelected :: Sample -> Views Int -> ViewsD Int
demandSelected s (M.PairT ints integers) = M.PairD (group ints) (group integers)
  where
    group root = let M.PairT first second = M.tval root in M.Need (M.PairD (selected first) (selected second))
    selected root = let M.PairT a b = M.tval root in M.Need (M.PairD
        (if leftDemand s then M.fullDemand a else M.Skip)
        (if rightDemand s then M.fullDemand b else M.Skip))

names :: Views a -> M.MemoristM [[M.ThName]]
names (M.PairT ints integers) = (++) <$> group ints <*> group integers
  where
    group outer = do
        M.PairT first second <- M.force outer
        sequence [leaves first, leaves second]
    leaves root = do
        M.PairT a b <- M.force root
        pure [M.tname root, M.tname a, M.tname b]

valid :: [[M.ThName]] -> Bool
valid [a,b,c,d] = a == b && c == d && length a == 3 && length c == 3
    && distinct a && distinct c && all (`notElem` c) a
  where
    distinct [] = True
    distinct (x:xs) = x `notElem` xs && distinct xs
valid _ = False
