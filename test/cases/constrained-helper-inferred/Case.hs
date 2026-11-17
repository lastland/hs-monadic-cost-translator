module Case (probes) where

import qualified Example as Source
import Generated (Scalar, oneM, twiceM)
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, failure, scalarSource)

probes :: [Probe]
probes =
    [ scalarSource "one/int"
        (sourceSum . Source.one . number) $ \sample -> do
        input <- M.lazy (pure (number sample))
        generic input >>= forceSum
    , scalarSource "one/integer"
        (fromInteger . sourceSum . Source.one . toInteger . number) $ \sample -> do
        input <- M.lazy (pure (toInteger (number sample)))
        fromInteger <$> (generic input >>= forceSum)
    , demanded "one/selected-fields" projectSelected demandSelected $ \sample -> do
        input <- M.lazy (pure (number sample))
        generic input
    , demanded "one/bottom-input-skipped" (\_ _ -> ())
        (\_ _ -> M.PairD M.Skip M.Skip) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        generic input
    , failure "one/bottom-input-demanded" $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        generic input >>= forceSum
    , scalarSource "twice/int"
        (sourceTwiceSum . Source.twice . number) $ \sample -> do
        input <- M.lazy (pure (number sample))
        genericTwice input >>= forceTwiceSum
    , scalarSource "twice/integer"
        (fromInteger . sourceTwiceSum . Source.twice . toInteger . number) $ \sample -> do
        input <- M.lazy (pure (toInteger (number sample)))
        fromInteger <$> (genericTwice input >>= forceTwiceSum)
    , demanded "twice/selected-helpers" projectHelpers demandHelpers $ \sample -> do
        input <- M.lazy (pure (number sample))
        genericTwice input
    , demanded "twice/helper-roots" (\_ _ -> ())
        (\_ _ -> M.PairD (M.Need (M.PairD M.Skip M.Skip)) (M.Need (M.PairD M.Skip M.Skip))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        genericTwice input
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
