module Case (probes) where

import Control.Monad ((>=>), replicateM)
import Data.Maybe (fromMaybe)
import qualified Example as Source
import Generated (Scalar, oneM, brokenAbstractM)
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, failure, scalar, scalarSource, snapshot)

probes :: [Probe]
probes =
    [ scalarSource "just-tuple/int/full-unused-input"
        (sourceViewsSum fromIntegral . Source.one (undefined :: Int) . other) $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        fixed <- M.lazy (pure (other sample))
        generic input fixed >>= forceSum
    , scalarSource "just-tuple/integer/full-unused-input"
        (fromInteger . sourceViewsSum fromIntegral . Source.one (undefined :: Integer) . other) $ \sample -> do
        input <- M.lazy (pure (undefined :: Integer))
        fixed <- M.lazy (pure (other sample))
        fromInteger <$> (generic input fixed >>= forceSum)
    , demanded "selected/maybe-roots" projectRoots demandRoots selectedViews
    , demanded "selected/tuple-roots"
        (projectTuples False False) (demandTuples False False) selectedViews
    , demanded "selected/only-abstract-leaves"
        (projectTuples True False) (demandTuples True False) selectedViews
    , demanded "selected/only-captured-fields"
        (projectTuples False True) (demandTuples False True) selectedViews
    , snapshot "just-tuple/aliases-and-caller-captured-field" $ \_ -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        fixed <- M.lazy (pure (undefined :: Int))
        names <- generic input fixed >>= fieldNames
        pure (case names of
            Just (first@[_, _, left, right], second) ->
                first == second && left /= right && right == M.tname fixed
            _ -> False)
    , scalarSource "just-tuple/abstract-only-unconstrained-function-field"
        (const (sourceViewsSum (const 0) (Source.one (undefined :: Int) (undefined :: Int -> Int)))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        fixed <- M.lazy (pure (undefined :: Int -> Int))
        generic input fixed >>= forceAbstractSum
    , scalarSource "just-tuple/repeated-fresh-calls"
        (\sample -> repeats sample * sourceViewsSum fromIntegral (Source.one (undefined :: Int) (other sample))) $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        fixed <- M.lazy (pure (other sample))
        sum <$> replicateM (repeats sample) (generic input fixed >>= forceSum)
    , scalarSource "just-tuple/repeated-cached-call"
        (\sample -> repeats sample * sourceViewsSum fromIntegral (Source.one (undefined :: Int) (other sample))) $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        fixed <- M.lazy (pure (other sample))
        cached <- M.lazy (generic input fixed)
        sum <$> replicateM (repeats sample) (M.force cached >>= forceSum)
    , snapshot "just-tuple/fresh-local-identities-and-shared-caller-field" freshIdentities
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
    , demanded "bottom-captured/both-tuple-roots"
        (projectTuples False False . bothDemanded) (demandTuples False False . bothDemanded) bottomCapturedViews
    , demanded "bottom-captured/both-abstract-leaves"
        (projectLeafSum True False . bothDemanded) (demandTuples True False . bothDemanded) bottomCapturedViews
    , failure "bottom-captured/captured-fields" (bottomCapturedViews >=> forceCapturedSum)
    , failure "bottom-captured/full" (bottomCapturedViews >=> forceSum)
    , demanded "broken-abstract/both-maybe-roots"
        (projectRoots . bothDemanded) (demandRoots . bothDemanded) brokenAbstractViews
    , demanded "broken-abstract/both-tuple-roots"
        (projectTuples False False . bothDemanded) (demandTuples False False . bothDemanded) brokenAbstractViews
    , demanded "broken-abstract/both-captured-fields"
        (projectLeafSum False True . bothDemanded) (demandTuples False True . bothDemanded) brokenAbstractViews
    , failure "broken-abstract/abstract-leaves" (brokenAbstractViews >=> forceAbstractSum)
    , failure "broken-abstract/full" (brokenAbstractViews >=> forceSum)
    , demanded "broken-abstract/both-paid-captured-fields"
        (projectLeafSum False True . bothDemanded) (demandTuples False True . bothDemanded) $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        fixed <- M.lazy (M.tick >> pure (other sample))
        genericBrokenAbstract input fixed
    ]

generic :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th b -> M.MemoristM (M.PairT (M.MaybeT (M.PairT a b)) (M.MaybeT (M.PairT a b)))
generic = oneM

sourceViewsSum :: Num a => (b -> a) -> (Maybe (a, b), Maybe (a, b)) -> a
sourceViewsSum capturedValue (first, second) = payloadSum first + payloadSum second
  where
    payloadSum Nothing = 0
    payloadSum (Just (left, right)) = left + capturedValue right

forceSum :: Num a => M.PairT (M.MaybeT (M.PairT a Int)) (M.MaybeT (M.PairT a Int)) -> M.MemoristM a
forceSum (M.PairT first second) = do
    left <- M.force first >>= payloadSum
    right <- M.force second >>= payloadSum
    pure (left + right)
  where
    payloadSum M.NothingT = pure 0
    payloadSum (M.JustT tuple) = do
        M.PairT left right <- M.force tuple
        firstValue <- M.force left
        secondValue <- M.force right
        pure (firstValue + fromIntegral secondValue)

forceAbstractSum :: Num a => M.PairT (M.MaybeT (M.PairT a b)) (M.MaybeT (M.PairT a b)) -> M.MemoristM a
forceAbstractSum (M.PairT first second) = do
    left <- M.force first >>= payloadValue
    right <- M.force second >>= payloadValue
    pure (left + right)
  where
    payloadValue M.NothingT = pure 0
    payloadValue (M.JustT tuple) = do
        M.PairT left _ <- M.force tuple
        M.force left

selectedViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (M.PairT Int Int)) (M.MaybeT (M.PairT Int Int)))
selectedViews sample = do
    input <- M.lazy (pure (undefined :: Int))
    fixed <- M.lazy (pure (other sample))
    generic input fixed

projectRoots :: Sample -> M.PairT (M.MaybeT (M.PairT Int Int)) (M.MaybeT (M.PairT Int Int)) -> (Maybe Bool, Maybe Bool)
projectRoots sample (M.PairT first second) =
    (project (leftDemand sample) first, project (rightDemand sample) second)
  where
    project False _ = Nothing
    project True root = Just (case M.tval root of
        M.NothingT -> False
        M.JustT _ -> True)

demandRoots :: Sample -> M.PairT (M.MaybeT (M.PairT Int Int)) (M.MaybeT (M.PairT Int Int)) -> M.PairD (M.MaybeD (M.PairD Int Int)) (M.MaybeD (M.PairD Int Int))
demandRoots sample (M.PairT first second) = M.PairD
    (demand (leftDemand sample) first) (demand (rightDemand sample) second)
  where
    demand False _ = M.Skip
    demand True root = M.Need (case M.tval root of
        M.NothingT -> M.NothingD
        M.JustT _ -> M.JustD M.Skip)

projectTuples :: Bool -> Bool -> Sample -> M.PairT (M.MaybeT (M.PairT Int Int)) (M.MaybeT (M.PairT Int Int)) -> (Maybe (Maybe (Maybe Int, Maybe Int)), Maybe (Maybe (Maybe Int, Maybe Int)))
projectTuples wantLeft wantRight sample (M.PairT first second) =
    (project (leftDemand sample) first, project (rightDemand sample) second)
  where
    project False _ = Nothing
    project True root = Just (case M.tval root of
        M.NothingT -> Nothing
        M.JustT tuple -> Just (tupleView (M.tval tuple)))
    tupleView (M.PairT left right) =
        (if wantLeft then Just (M.tval left) else Nothing,
         if wantRight then Just (M.tval right) else Nothing)

demandTuples :: Bool -> Bool -> Sample -> M.PairT (M.MaybeT (M.PairT Int Int)) (M.MaybeT (M.PairT Int Int)) -> M.PairD (M.MaybeD (M.PairD Int Int)) (M.MaybeD (M.PairD Int Int))
demandTuples wantLeft wantRight sample (M.PairT first second) = M.PairD
    (demand (leftDemand sample) first) (demand (rightDemand sample) second)
  where
    demand False _ = M.Skip
    demand True root = M.Need (case M.tval root of
        M.NothingT -> M.NothingD
        M.JustT tuple -> M.JustD (M.Need (tupleDemand (M.tval tuple))))
    tupleDemand (M.PairT left right) = M.PairD
        (if wantLeft then M.Need (M.tval left) else M.Skip)
        (if wantRight then M.Need (M.tval right) else M.Skip)

fieldNames :: M.PairT (M.MaybeT (M.PairT a b)) (M.MaybeT (M.PairT a b)) -> M.MemoristM (Maybe ([M.ThName], [M.ThName]))
fieldNames (M.PairT first second) = do
    left <- M.force first
    right <- M.force second
    case (left, right) of
        (M.JustT leftTuple, M.JustT rightTuple) -> do
            M.PairT leftFirst leftSecond <- M.force leftTuple
            M.PairT rightFirst rightSecond <- M.force rightTuple
            pure (Just
                ( [M.tname first, M.tname leftTuple, M.tname leftFirst, M.tname leftSecond]
                , [M.tname second, M.tname rightTuple, M.tname rightFirst, M.tname rightSecond]
                ))
        _ -> pure Nothing

freshIdentities :: Sample -> Bool
freshIdentities _ = M.value $ M.eval1 $ do
    input <- M.lazy (pure (undefined :: Int))
    fixed <- M.lazy (pure (undefined :: Int))
    firstNames <- generic input fixed >>= fieldNames
    secondNames <- generic input fixed >>= fieldNames
    pure (case (firstNames, secondNames) of
        (Just (first@[root, tuple, left, right], firstAlias),
         Just (second@[nextRoot, nextTuple, nextLeft, nextRight], secondAlias)) ->
            first == firstAlias && second == secondAlias &&
            left /= right && nextLeft /= nextRight &&
            right == M.tname fixed && nextRight == M.tname fixed &&
            all (`notElem` [nextRoot, nextTuple, nextLeft]) [root, tuple, left]
        _ -> False)

forceCapturedSum :: M.PairT (M.MaybeT (M.PairT a Int)) (M.MaybeT (M.PairT a Int)) -> M.MemoristM Int
forceCapturedSum (M.PairT first second) = do
    left <- M.force first >>= payloadValue
    right <- M.force second >>= payloadValue
    pure (left + right)
  where
    payloadValue M.NothingT = pure 0
    payloadValue (M.JustT tuple) = do
        M.PairT _ right <- M.force tuple
        M.force right

genericBrokenAbstract :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th b -> M.MemoristM (M.PairT (M.MaybeT (M.PairT a b)) (M.MaybeT (M.PairT a b)))
genericBrokenAbstract = brokenAbstractM

bothDemanded :: Sample -> Sample
bothDemanded sample = sample {leftDemand = True, rightDemand = True}

bottomCapturedViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (M.PairT Int Int)) (M.MaybeT (M.PairT Int Int)))
bottomCapturedViews _ = do
    input <- M.lazy (pure (undefined :: Int))
    fixed <- M.lazy (pure (undefined :: Int))
    generic input fixed

brokenAbstractViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (M.PairT Int Int)) (M.MaybeT (M.PairT Int Int)))
brokenAbstractViews sample = do
    input <- M.lazy (pure (undefined :: Int))
    fixed <- M.lazy (pure (other sample))
    genericBrokenAbstract input fixed

projectLeafSum :: Bool -> Bool -> Sample -> M.PairT (M.MaybeT (M.PairT Int Int)) (M.MaybeT (M.PairT Int Int)) -> Int
projectLeafSum wantLeft wantRight sample views = case projectTuples wantLeft wantRight sample views of
    (first, second) -> payloadSum first + payloadSum second
  where
    payloadSum (Just (Just (left, right))) = fromMaybe 0 left + fromMaybe 0 right
    payloadSum _ = 0
