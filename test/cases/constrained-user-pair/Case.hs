module Case (probes) where

import Control.Monad ((>=>), replicateM)
import Data.Maybe (fromMaybe)
import qualified Example as Source
import Generated (PairT(..), PairD(..), Scalar, oneM, brokenAbstractM, brokenPairM, twinsM)
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, failure, scalar, scalarSource, snapshot)

probes :: [Probe]
probes =
    [ scalarSource "just-pair/int/full-unused-input"
        (sourceViewsSum fromIntegral . Source.one (undefined :: Int) . other) $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        fixed <- M.lazy (pure (other sample))
        generic input fixed >>= forceSum
    , scalarSource "just-pair/integer/full-unused-input"
        (fromInteger . sourceViewsSum fromIntegral . Source.one (undefined :: Integer) . other) $ \sample -> do
        input <- M.lazy (pure (undefined :: Integer))
        fixed <- M.lazy (pure (other sample))
        fromInteger <$> (generic input fixed >>= forceSum)
    , demanded "selected/maybe-roots" projectRoots demandRoots selectedViews
    , demanded "selected/pair-roots"
        (projectPairs False False) (demandPairs False False) selectedViews
    , demanded "selected/only-abstract-leaves"
        (projectPairs True False) (demandPairs True False) selectedViews
    , demanded "selected/only-captured-fields"
        (projectPairs False True) (demandPairs False True) selectedViews
    , snapshot "just-pair/aliases-and-caller-captured-field" $ \_ -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        fixed <- M.lazy (pure (undefined :: Int))
        names <- generic input fixed >>= fieldNames
        pure (case names of
            Just (first@[_, _, left, right], second) ->
                first == second && left /= right && right == M.tname fixed
            _ -> False)
    , scalarSource "just-pair/abstract-only-unconstrained-function-field"
        (const (sourceViewsSum (const 0) (Source.one (undefined :: Int) (undefined :: Int -> Int)))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        fixed <- M.lazy (pure (undefined :: Int -> Int))
        generic input fixed >>= forceAbstractSum
    , scalarSource "just-pair/repeated-fresh-calls"
        (\sample -> repeats sample * sourceViewsSum fromIntegral (Source.one (undefined :: Int) (other sample))) $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        fixed <- M.lazy (pure (other sample))
        sum <$> replicateM (repeats sample) (generic input fixed >>= forceSum)
    , scalarSource "just-pair/repeated-cached-call"
        (\sample -> repeats sample * sourceViewsSum fromIntegral (Source.one (undefined :: Int) (other sample))) $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        fixed <- M.lazy (pure (other sample))
        cached <- M.lazy (generic input fixed)
        sum <$> replicateM (repeats sample) (M.force cached >>= forceSum)
    , snapshot "just-pair/fresh-local-identities-and-shared-caller-field" freshIdentities
    , scalarSource "paid-captured/full/repeated-fresh-calls"
        (\sample -> repeats sample * sourceViewsSum fromIntegral (Source.one (undefined :: Int) (other sample))) $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        fixed <- M.lazy (M.tick >> pure (other sample))
        sum <$> replicateM (repeats sample) (generic input fixed >>= forceSum)
    , scalarSource "paid-captured/full/repeated-cached-call"
        (\sample -> repeats sample * sourceViewsSum fromIntegral (Source.one (undefined :: Int) (other sample))) $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        fixed <- M.lazy (M.tick >> pure (other sample))
        cached <- M.lazy (generic input fixed)
        sum <$> replicateM (repeats sample) (M.force cached >>= forceSum)
    , scalar "paid-captured/only-captured/repeated-fresh-calls" $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        fixed <- M.lazy (M.tick >> pure (other sample))
        sum <$> replicateM (repeats sample) (generic input fixed >>= forceCapturedSum)
    , scalar "paid-captured/only-captured/repeated-cached-call" $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        fixed <- M.lazy (M.tick >> pure (other sample))
        cached <- M.lazy (generic input fixed)
        sum <$> replicateM (repeats sample) (M.force cached >>= forceCapturedSum)
    , demanded "bottom-captured/both-maybe-roots"
        (projectRoots . bothDemanded) (demandRoots . bothDemanded) bottomCapturedViews
    , demanded "bottom-captured/both-pair-roots"
        (projectPairs False False . bothDemanded) (demandPairs False False . bothDemanded) bottomCapturedViews
    , demanded "bottom-captured/both-abstract-leaves"
        (projectLeafSum True False . bothDemanded) (demandPairs True False . bothDemanded) bottomCapturedViews
    , failure "bottom-captured/captured-fields" (bottomCapturedViews >=> forceCapturedSum)
    , failure "bottom-captured/full" (bottomCapturedViews >=> forceSum)
    , demanded "broken-abstract/both-maybe-roots"
        (projectRoots . bothDemanded) (demandRoots . bothDemanded) brokenAbstractViews
    , demanded "broken-abstract/both-pair-roots"
        (projectPairs False False . bothDemanded) (demandPairs False False . bothDemanded) brokenAbstractViews
    , demanded "broken-abstract/both-captured-fields"
        (projectLeafSum False True . bothDemanded) (demandPairs False True . bothDemanded) brokenAbstractViews
    , failure "broken-abstract/abstract-leaves" (brokenAbstractViews >=> forceAbstractSum)
    , failure "broken-abstract/full" (brokenAbstractViews >=> forceSum)
    , demanded "broken-abstract/both-paid-captured-fields"
        (projectLeafSum False True . bothDemanded) (demandPairs False True . bothDemanded) $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        fixed <- M.lazy (M.tick >> pure (other sample))
        genericBrokenAbstract input fixed
    , demanded "broken-pair/both-maybe-roots"
        (projectRoots . bothDemanded) (demandRoots . bothDemanded) brokenPairViews
    , failure "broken-pair/pair-roots" (brokenPairViews >=> forcePairRoots)
    , failure "broken-pair/full" (brokenPairViews >=> forceSum)
    , scalarSource "twins/int/full-unused-input"
        (const (sourceViewsSum fromIntegral (Source.twins (undefined :: Int)))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        genericTwins input >>= forceTwinsSum
    , scalarSource "twins/integer/full-unused-input"
        (const (fromInteger (sourceViewsSum id (Source.twins (undefined :: Integer))))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Integer))
        fromInteger <$> (genericTwins input >>= forceTwinsSum)
    , demanded "twins/both-left-fields"
        (projectLeafSum True False . bothDemanded) (demandPairs True False . bothDemanded) twinsViews
    , demanded "twins/both-right-fields"
        (projectLeafSum False True . bothDemanded) (demandPairs False True . bothDemanded) twinsViews
    , snapshot "twins/shared-views-and-distinct-equal-rhs-fields" $ \_ -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        names <- genericTwins input >>= fieldNames
        pure (case names of
            Just (first@[_, _, left, right], second) -> first == second && left /= right
            _ -> False)
    ]

generic :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th b -> M.MemoristM (M.PairT (M.MaybeT (PairT a b)) (M.MaybeT (PairT a b)))
generic = oneM

sourceViewsSum :: Num a => (b -> a) -> (Maybe (Source.Pair a b), Maybe (Source.Pair a b)) -> a
sourceViewsSum capturedValue (first, second) = payloadSum first + payloadSum second
  where
    payloadSum Nothing = 0
    payloadSum (Just (Source.Pair left right)) = left + capturedValue right

forceSum :: Num a => M.PairT (M.MaybeT (PairT a Int)) (M.MaybeT (PairT a Int)) -> M.MemoristM a
forceSum (M.PairT first second) = do
    left <- M.force first >>= payloadSum
    right <- M.force second >>= payloadSum
    pure (left + right)
  where
    payloadSum M.NothingT = pure 0
    payloadSum (M.JustT pair) = do
        PairT left right <- M.force pair
        firstValue <- M.force left
        secondValue <- M.force right
        pure (firstValue + fromIntegral secondValue)

forceAbstractSum :: Num a => M.PairT (M.MaybeT (PairT a b)) (M.MaybeT (PairT a b)) -> M.MemoristM a
forceAbstractSum (M.PairT first second) = do
    left <- M.force first >>= payloadValue
    right <- M.force second >>= payloadValue
    pure (left + right)
  where
    payloadValue M.NothingT = pure 0
    payloadValue (M.JustT pair) = do
        PairT left _ <- M.force pair
        M.force left

selectedViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (PairT Int Int)) (M.MaybeT (PairT Int Int)))
selectedViews sample = do
    input <- M.lazy (pure (undefined :: Int))
    fixed <- M.lazy (pure (other sample))
    generic input fixed

projectRoots :: Sample -> M.PairT (M.MaybeT (PairT Int Int)) (M.MaybeT (PairT Int Int)) -> (Maybe Bool, Maybe Bool)
projectRoots sample (M.PairT first second) =
    (project (leftDemand sample) first, project (rightDemand sample) second)
  where
    project False _ = Nothing
    project True root = Just (case M.tval root of
        M.NothingT -> False
        M.JustT _ -> True)

demandRoots :: Sample -> M.PairT (M.MaybeT (PairT Int Int)) (M.MaybeT (PairT Int Int)) -> M.PairD (M.MaybeD (PairD Int Int)) (M.MaybeD (PairD Int Int))
demandRoots sample (M.PairT first second) = M.PairD
    (demand (leftDemand sample) first) (demand (rightDemand sample) second)
  where
    demand False _ = M.Skip
    demand True root = M.Need (case M.tval root of
        M.NothingT -> M.NothingD
        M.JustT _ -> M.JustD M.Skip)

projectPairs :: Bool -> Bool -> Sample -> M.PairT (M.MaybeT (PairT Int Int)) (M.MaybeT (PairT Int Int)) -> (Maybe (Maybe (Maybe Int, Maybe Int)), Maybe (Maybe (Maybe Int, Maybe Int)))
projectPairs wantLeft wantRight sample (M.PairT first second) =
    (project (leftDemand sample) first, project (rightDemand sample) second)
  where
    project False _ = Nothing
    project True root = Just (case M.tval root of
        M.NothingT -> Nothing
        M.JustT pair -> Just (pairView (M.tval pair)))
    pairView (PairT left right) =
        (if wantLeft then Just (M.tval left) else Nothing,
         if wantRight then Just (M.tval right) else Nothing)

demandPairs :: Bool -> Bool -> Sample -> M.PairT (M.MaybeT (PairT Int Int)) (M.MaybeT (PairT Int Int)) -> M.PairD (M.MaybeD (PairD Int Int)) (M.MaybeD (PairD Int Int))
demandPairs wantLeft wantRight sample (M.PairT first second) = M.PairD
    (demand (leftDemand sample) first) (demand (rightDemand sample) second)
  where
    demand False _ = M.Skip
    demand True root = M.Need (case M.tval root of
        M.NothingT -> M.NothingD
        M.JustT pair -> M.JustD (M.Need (pairDemand (M.tval pair))))
    pairDemand (PairT left right) = PairD
        (if wantLeft then M.Need (M.tval left) else M.Skip)
        (if wantRight then M.Need (M.tval right) else M.Skip)

fieldNames :: M.PairT (M.MaybeT (PairT a b)) (M.MaybeT (PairT a b)) -> M.MemoristM (Maybe ([M.ThName], [M.ThName]))
fieldNames (M.PairT first second) = do
    left <- M.force first
    right <- M.force second
    case (left, right) of
        (M.JustT leftPair, M.JustT rightPair) -> do
            PairT leftFirst leftSecond <- M.force leftPair
            PairT rightFirst rightSecond <- M.force rightPair
            pure (Just
                ( [M.tname first, M.tname leftPair, M.tname leftFirst, M.tname leftSecond]
                , [M.tname second, M.tname rightPair, M.tname rightFirst, M.tname rightSecond]
                ))
        _ -> pure Nothing

freshIdentities :: Sample -> Bool
freshIdentities _ = M.value $ M.eval1 $ do
    input <- M.lazy (pure (undefined :: Int))
    fixed <- M.lazy (pure (undefined :: Int))
    firstNames <- generic input fixed >>= fieldNames
    secondNames <- generic input fixed >>= fieldNames
    pure (case (firstNames, secondNames) of
        (Just (first@[root, pair, left, right], firstAlias),
         Just (second@[nextRoot, nextPair, nextLeft, nextRight], secondAlias)) ->
            first == firstAlias && second == secondAlias &&
            left /= right && nextLeft /= nextRight &&
            right == M.tname fixed && nextRight == M.tname fixed &&
            all (`notElem` [nextRoot, nextPair, nextLeft]) [root, pair, left]
        _ -> False)

forceCapturedSum :: Num b => M.PairT (M.MaybeT (PairT a b)) (M.MaybeT (PairT a b)) -> M.MemoristM b
forceCapturedSum (M.PairT first second) = do
    left <- M.force first >>= payloadValue
    right <- M.force second >>= payloadValue
    pure (left + right)
  where
    payloadValue M.NothingT = pure 0
    payloadValue (M.JustT pair) = do
        PairT _ right <- M.force pair
        M.force right

genericBrokenAbstract :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th b -> M.MemoristM (M.PairT (M.MaybeT (PairT a b)) (M.MaybeT (PairT a b)))
genericBrokenAbstract = brokenAbstractM

genericBrokenPair :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th b -> M.MemoristM (M.PairT (M.MaybeT (PairT a b)) (M.MaybeT (PairT a b)))
genericBrokenPair = brokenPairM

genericTwins :: (Eq a, Num a, Scalar a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (PairT a a)) (M.MaybeT (PairT a a)))
genericTwins = twinsM

bothDemanded :: Sample -> Sample
bothDemanded sample = sample {leftDemand = True, rightDemand = True}

bottomCapturedViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (PairT Int Int)) (M.MaybeT (PairT Int Int)))
bottomCapturedViews _ = do
    input <- M.lazy (pure (undefined :: Int))
    fixed <- M.lazy (pure (undefined :: Int))
    generic input fixed

brokenAbstractViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (PairT Int Int)) (M.MaybeT (PairT Int Int)))
brokenAbstractViews sample = do
    input <- M.lazy (pure (undefined :: Int))
    fixed <- M.lazy (pure (other sample))
    genericBrokenAbstract input fixed

brokenPairViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (PairT Int Int)) (M.MaybeT (PairT Int Int)))
brokenPairViews sample = do
    input <- M.lazy (pure (undefined :: Int))
    fixed <- M.lazy (pure (other sample))
    genericBrokenPair input fixed

projectLeafSum :: Bool -> Bool -> Sample -> M.PairT (M.MaybeT (PairT Int Int)) (M.MaybeT (PairT Int Int)) -> Int
projectLeafSum wantLeft wantRight sample views = case projectPairs wantLeft wantRight sample views of
    (first, second) -> payloadSum first + payloadSum second
  where
    payloadSum (Just (Just (left, right))) = fromMaybe 0 left + fromMaybe 0 right
    payloadSum _ = 0

forcePairRoots :: M.PairT (M.MaybeT (PairT a b)) (M.MaybeT (PairT a b)) -> M.MemoristM Int
forcePairRoots (M.PairT first second) = do
    left <- M.force first >>= payloadRoot
    right <- M.force second >>= payloadRoot
    pure (left + right)
  where
    payloadRoot M.NothingT = pure 0
    payloadRoot (M.JustT pair) = do
        PairT _ _ <- M.force pair
        pure 1

twinsViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (PairT Int Int)) (M.MaybeT (PairT Int Int)))
twinsViews _ = do
    input <- M.lazy (pure (undefined :: Int))
    genericTwins input

forceTwinsSum :: Num a => M.PairT (M.MaybeT (PairT a a)) (M.MaybeT (PairT a a)) -> M.MemoristM a
forceTwinsSum views = (+) <$> forceAbstractSum views <*> forceCapturedSum views
