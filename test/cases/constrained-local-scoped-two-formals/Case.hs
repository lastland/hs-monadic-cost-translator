module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, failure, scalarSource, snapshot)

type Views a b = M.PairT (M.PairT a b) (M.PairT a b)

probes :: [Probe]
probes =
    [ scalarSource "full/int-integer" (sourceSum True True . positive) $ \s ->
        call (undefined :: Int) (undefined :: Integer) (positive s) >>= forceCopies True True
    , scalarSource "full/integer-int" (sourceSum True True . positive) $ \s ->
        call (undefined :: Integer) (undefined :: Int) (positive s) >>= forceCopies True True
    , scalarSource "left/right-bottom-skipped" (const (sourceSum True False 0)) $ \_ ->
        callInts 0 >>= forceCopies True False
    , scalarSource "right/left-bottom-skipped" (const (sourceSum False True (-1))) $ \_ ->
        callInts (-1) >>= forceCopies False True
    , demanded "skip-all/bottom-count" (\_ _ -> ()) (\_ _ -> M.PairD M.Skip M.Skip)
        (const (callInts undefined))
    , demanded "roots-only/bottom-count" (\_ _ -> ()) (\_ _ -> rootDemand)
        (const (callInts undefined))
    , demanded "selected-leaves" projectSelected demandSelected (callInts . positive)
    , failure "left/demanded-negative" (\_ -> callInts (-1) >>= forceCopies True False)
    , failure "right/demanded-zero" (\_ -> callInts 0 >>= forceCopies False True)
    , failure "leaf/demanded-bottom-count" (\_ -> callInts undefined >>= forceCopies True False)
    , scalarSource "repeated/uncached" (\s -> repeats s * sourceSum True True (positive s)) $ \s -> do
        a <- M.lazy (pure (undefined :: Int))
        b <- M.lazy (pure (undefined :: Integer))
        count <- M.lazy (pure (positive s))
        sum <$> replicateM (repeats s) (generic a b count >>= forceCopies True True)
    , scalarSource "repeated/cached" (\s -> repeats s * sourceSum True True (positive s)) $ \s -> do
        cached <- M.lazy (callInts (positive s))
        sum <$> replicateM (repeats s) (M.force cached >>= forceCopies True True)
    , snapshot "identity/root-and-payload-aliases" $ \s -> M.value $ M.eval1 $ do
        groups <- callInts (positive s) >>= names
        pure (valid groups)
    , snapshot "identity/fresh-invocations" $ \s -> M.value $ M.eval1 $ do
        a <- M.lazy (pure (undefined :: Int))
        b <- M.lazy (pure (undefined :: Integer))
        count <- M.lazy (pure (positive s))
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
callInts = call (undefined :: Int) (undefined :: Integer)

positive :: Sample -> Int
positive = (1 +) . abs . number

sourceSum :: Bool -> Bool -> Int -> Int
sourceSum l r n = selected first + selected second
  where
    (first, second) = Source.views (undefined :: Int) (undefined :: Integer) n
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
