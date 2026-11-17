module Case (probes) where

import qualified Example as Source
import Generated (Scalar, oneM, twiceM)
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, scalarSource, snapshot)

probes :: [Probe]
probes =
    [ scalarSource "one/int"
        (const (sourceSum (Source.one (undefined :: Int)))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        generic input >>= forceSum
    , scalarSource "one/integer"
        (const (fromInteger (sourceSum (Source.one (undefined :: Integer))))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Integer))
        fromInteger <$> (generic input >>= forceSum)
    , demanded "one/selected-fields" projectSelected demandSelected $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        generic input
    , snapshot "one/field-aliases" $ \_ -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        M.PairT first second <- generic input
        pure (M.tname first == M.tname second)
    , snapshot "one/fresh-invocations" $ \_ -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        M.PairT firstLeft firstRight <- generic input
        M.PairT secondLeft secondRight <- generic input
        pure (all (`notElem` [M.tname secondLeft, M.tname secondRight])
            [M.tname firstLeft, M.tname firstRight])
    , scalarSource "twice/int"
        (const (sourceTwiceSum (Source.twice (undefined :: Int)))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        genericTwice input >>= forceTwiceSum
    , scalarSource "twice/integer"
        (const (fromInteger (sourceTwiceSum (Source.twice (undefined :: Integer))))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Integer))
        fromInteger <$> (genericTwice input >>= forceTwiceSum)
    , demanded "twice/selected-helpers" projectHelpers demandHelpers $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        genericTwice input
    , demanded "twice/helper-roots" (\_ _ -> ())
        (\_ _ -> M.PairD (M.Need (M.PairD M.Skip M.Skip)) (M.Need (M.PairD M.Skip M.Skip))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        genericTwice input
    , snapshot "twice/field-aliases" $ \_ -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        M.PairT first second <- genericTwice input
        M.PairT firstLeft firstRight <- M.force first
        M.PairT secondLeft secondRight <- M.force second
        pure (M.tname firstLeft == M.tname firstRight &&
            M.tname secondLeft == M.tname secondRight)
    , snapshot "twice/fresh-helper-invocations" $ \_ -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        M.PairT first second <- genericTwice input
        M.PairT firstLeft firstRight <- M.force first
        M.PairT secondLeft secondRight <- M.force second
        pure (all (`notElem` [M.tname secondLeft, M.tname secondRight])
            [M.tname firstLeft, M.tname firstRight])
    ]

generic :: (Eq a, Num a, Scalar a) => M.Th a -> M.MemoristM (M.PairT a a)
generic = oneM

sourceSum :: Num a => (a, a) -> a
sourceSum (first, second) = first + second

forceSum :: Num a => M.PairT a a -> M.MemoristM a
forceSum (M.PairT first second) = do
    a <- M.force first
    b <- M.force second
    pure (a + b)

projectSelected :: Sample -> M.PairT Int Int -> (Maybe Int, Maybe Int)
projectSelected sample (M.PairT first second) =
    (if leftDemand sample then Just (M.tval first) else Nothing,
     if rightDemand sample then Just (M.tval second) else Nothing)

demandSelected :: Sample -> M.PairT Int Int -> M.PairD Int Int
demandSelected sample (M.PairT first second) = M.PairD
    (if leftDemand sample then M.fullDemand first else M.Skip)
    (if rightDemand sample then M.fullDemand second else M.Skip)

genericTwice :: (Eq a, Num a, Scalar a) => M.Th a -> M.MemoristM (M.PairT (M.PairT a a) (M.PairT a a))
genericTwice = twiceM

sourceTwiceSum :: Num a => ((a, a), (a, a)) -> a
sourceTwiceSum (first, second) = sourceSum first + sourceSum second

forceTwiceSum :: Num a => M.PairT (M.PairT a a) (M.PairT a a) -> M.MemoristM a
forceTwiceSum (M.PairT first second) = do
    a <- M.force first >>= forceSum
    b <- M.force second >>= forceSum
    pure (a + b)

pairValues :: M.PairT Int Int -> (Int, Int)
pairValues (M.PairT first second) = (M.tval first, M.tval second)

projectHelpers :: Sample -> M.PairT (M.PairT Int Int) (M.PairT Int Int) -> (Maybe (Int, Int), Maybe (Int, Int))
projectHelpers sample (M.PairT first second) =
    (if leftDemand sample then Just (pairValues (M.tval first)) else Nothing,
     if rightDemand sample then Just (pairValues (M.tval second)) else Nothing)

demandHelpers :: Sample -> M.PairT (M.PairT Int Int) (M.PairT Int Int) -> M.PairD (M.PairD Int Int) (M.PairD Int Int)
demandHelpers sample (M.PairT first second) = M.PairD
    (if leftDemand sample then M.fullDemand first else M.Skip)
    (if rightDemand sample then M.fullDemand second else M.Skip)
