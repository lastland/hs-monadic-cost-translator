module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, scalarSource, snapshot)

type Views a b = M.PairT (M.PairT a b) (M.PairT a b)

probes :: [Probe]
probes =
    [ scalarSource "full/int-integer" (sourceSum True True . number) $ \s ->
        call (0 :: Int) (0 :: Integer) (number s) >>= forceCopies True True
    , scalarSource "full/integer-int" (sourceSum True True . number) $ \s ->
        call (0 :: Integer) (0 :: Int) (number s) >>= forceCopies True True
    , scalarSource "left/right-skipped" (const (sourceSum True False 0)) $ \_ ->
        callInts 0 >>= forceCopies True False
    , scalarSource "right/left-skipped" (const (sourceSum False True (-1))) $ \_ ->
        callInts (-1) >>= forceCopies False True
    , demanded "skip-all/paid-count-skipped" (\_ _ -> ()) (\_ _ -> M.PairD M.Skip M.Skip)
        (paidCount . number)
    , demanded "roots-only/paid-count-skipped" (\_ _ -> ()) (\_ _ -> rootDemand)
        (paidCount . number)
    , demanded "selected-leaves" projectSelected demandSelected (callInts . number)
    , scalarSource "repeated/int-integer/uncached" repeatedSource $ \s -> do
        a <- M.lazy (pure (0 :: Int))
        b <- M.lazy (pure (0 :: Integer))
        count <- M.lazy (pure (number s))
        sum <$> replicateM (repeats s) (generic a b count >>= forceCopies True True)
    , scalarSource "repeated/int-integer/cached" repeatedSource $ \s -> do
        cached <- M.lazy (callInts (number s))
        sum <$> replicateM (repeats s) (M.force cached >>= forceCopies True True)
    , scalarSource "repeated/integer-int/uncached" repeatedSource $ \s -> do
        a <- M.lazy (pure (0 :: Integer))
        b <- M.lazy (pure (0 :: Int))
        count <- M.lazy (pure (number s))
        sum <$> replicateM (repeats s) (generic a b count >>= forceCopies True True)
    , scalarSource "repeated/integer-int/cached" repeatedSource $ \s -> do
        cached <- M.lazy (call (0 :: Integer) (0 :: Int) (number s))
        sum <$> replicateM (repeats s) (M.force cached >>= forceCopies True True)
    , snapshot "identity/root-and-payload-aliases" $ \s -> M.value $ M.eval1 $ do
        groups <- callInts (number s) >>= names
        pure (valid groups)
    , snapshot "identity/fresh-invocations" $ \s -> M.value $ M.eval1 $ do
        a <- M.lazy (pure (0 :: Int))
        b <- M.lazy (pure (0 :: Integer))
        count <- M.lazy (pure (number s))
        first <- generic a b count >>= names
        second <- generic a b count >>= names
        pure (valid first && valid second && all (`notElem` concat second) (concat first))
    ]

-- Both formals remain abstract: no reflection, evaluator, or setup constraint.
generic :: (Num a, Num b) => M.Th a -> M.Th b -> M.Th Int -> M.MemoristM (Views a b)
generic = G.viewsM

call :: (Num a, Num b) => a -> b -> Int -> M.MemoristM (Views a b)
call a b count = do
    x <- M.lazy (pure a)
    y <- M.lazy (pure b)
    n <- M.lazy (pure count)
    generic x y n

callInts :: Int -> M.MemoristM (Views Int Integer)
callInts = call (0 :: Int) (0 :: Integer)

-- Skipping leaves must not charge the total captured count's extra tick.
paidCount :: Int -> M.MemoristM (Views Int Integer)
paidCount n = do
    a <- M.lazy (pure (0 :: Int))
    b <- M.lazy (pure (0 :: Integer))
    count <- M.lazy (M.tick >> pure n)
    generic a b count

repeatedSource :: Sample -> Int
repeatedSource s = repeats s * sourceSum True True (number s)

sourceSum :: Bool -> Bool -> Int -> Int
sourceSum l r n = selected first + selected second
  where
    (first, second) = Source.views (0 :: Int) (0 :: Integer) n
    selected (a,b) = (if l then a else 0) + (if r then fromInteger b else 0)

forceCopies :: (Integral a, Integral b) => Bool -> Bool -> Views a b -> M.MemoristM Int
forceCopies l r (M.PairT first second) = do
    a <- M.force first >>= selected
    b <- M.force second >>= selected
    pure (a+b)
  where
    selected (M.PairT a b) = do
        x <- if l then fromIntegral <$> M.force a else pure 0
        y <- if r then fromIntegral <$> M.force b else pure 0
        pure (x+y)

rootDemand :: M.PairD (M.PairD Int Integer) (M.PairD Int Integer)
rootDemand = M.PairD root root
  where root = M.Need (M.PairD M.Skip M.Skip)

projectSelected :: Sample -> Views Int Integer -> ((Maybe Int, Maybe Integer), (Maybe Int, Maybe Integer))
projectSelected s (M.PairT first second) = (selected (M.tval first), selected (M.tval second))
  where
    selected (M.PairT a b) =
        (if leftDemand s then Just (M.tval a) else Nothing,
         if rightDemand s then Just (M.tval b) else Nothing)

demandSelected :: Sample -> Views Int Integer -> M.PairD (M.PairD Int Integer) (M.PairD Int Integer)
demandSelected s (M.PairT first second) = M.PairD (selected first) (selected second)
  where
    selected root = let M.PairT a b = M.tval root in M.Need (M.PairD
        (if leftDemand s then M.fullDemand a else M.Skip)
        (if rightDemand s then M.fullDemand b else M.Skip))

names :: Views a b -> M.MemoristM [[M.ThName]]
names (M.PairT first second) = sequence [group first, group second]
  where
    group root = do
        M.PairT a b <- M.force root
        pure [M.tname root, M.tname a, M.tname b]

valid :: [[M.ThName]] -> Bool
valid [a,b] = a == b && length a == 3 && distinct a
  where
    distinct [] = True
    distinct (x:xs) = x `notElem` xs && distinct xs
valid _ = False
