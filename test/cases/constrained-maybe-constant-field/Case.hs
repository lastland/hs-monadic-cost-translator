module Case (probes) where

import Control.Monad ((>=>), replicateM)
import Data.Maybe (fromMaybe)
import qualified Example as Source
import Generated (Scalar, oneM, brokenTupleM, brokenAbstractM, brokenFixedM, paidM)
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, failure, scalarSource, snapshot)

probes :: [Probe]
probes =
    [ scalarSource "just-tuple/int/full-unused-count"
        (const (sourceSum (undefined :: Int) undefined)) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        generic input n >>= forceSum
    , scalarSource "just-tuple/integer/full-unused-count"
        (const (fromInteger (sourceSum (undefined :: Integer) undefined))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Integer))
        n <- M.lazy (pure (undefined :: Int))
        fromInteger <$> (generic input n >>= forceSum)
    , demanded "selected/maybe-roots" projectRoots demandRoots selectedViews
    , demanded "selected/tuple-roots"
        (projectTuples False False) (demandTuples False False) selectedViews
    , demanded "selected/only-abstract-leaves"
        (projectTuples True False) (demandTuples True False) selectedViews
    , demanded "selected/only-fixed-leaves"
        (projectTuples False True) (demandTuples False True) selectedViews
    , snapshot "just-tuple/maybe-tuple-and-distinct-leaf-aliases" $ \sample -> M.value $ M.eval1 $ do
        names <- selectedViews sample >>= fieldNames
        pure (case names of
            Just (first@[_, _, left, right], second) -> first == second && left /= right
            _ -> False)
    , scalarSource "just-tuple/repeated-fresh-calls"
        (\sample -> repeats sample * sourceSum (undefined :: Int) undefined) $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        sum <$> replicateM (repeats sample) (generic input n >>= forceSum)
    , scalarSource "just-tuple/repeated-cached-call"
        (\sample -> repeats sample * sourceSum (undefined :: Int) undefined) $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        cached <- M.lazy (generic input n)
        sum <$> replicateM (repeats sample) (M.force cached >>= forceSum)
    , snapshot "just-tuple/fresh-maybe-tuple-and-leaf-identities" freshIdentities
    , demanded "broken-tuple/both-maybe-roots"
        (projectRoots . bothDemanded) (demandRoots . bothDemanded) brokenTupleViews
    , failure "broken-tuple/tuple-roots" (brokenTupleViews >=> forceTupleRoots)
    , failure "broken-tuple/full" (brokenTupleViews >=> forceSum)
    , demanded "broken-abstract/both-tuple-roots"
        (projectTuples False False . bothDemanded) (demandTuples False False . bothDemanded) brokenAbstractViews
    , demanded "broken-abstract/both-fixed-leaves"
        (projectLeafSum False True . bothDemanded) (demandTuples False True . bothDemanded) brokenAbstractViews
    , failure "broken-abstract/abstract-leaves" (brokenAbstractViews >=> forceLeafSum True)
    , failure "broken-abstract/full" (brokenAbstractViews >=> forceSum)
    , demanded "broken-fixed/both-tuple-roots"
        (projectTuples False False . bothDemanded) (demandTuples False False . bothDemanded) brokenFixedViews
    , demanded "broken-fixed/both-abstract-leaves"
        (projectLeafSum True False . bothDemanded) (demandTuples True False . bothDemanded) brokenFixedViews
    , failure "broken-fixed/fixed-leaves" (brokenFixedViews >=> forceLeafSum False)
    , failure "broken-fixed/full" (brokenFixedViews >=> forceSum)
    , scalarSource "paid/int/full-unused-count"
        (const (paidSourceSum (undefined :: Int) undefined)) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        genericPaid input n >>= forceSum
    , scalarSource "paid/integer/full-unused-count"
        (const (fromInteger (paidSourceSum (undefined :: Integer) undefined))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Integer))
        n <- M.lazy (pure (undefined :: Int))
        fromInteger <$> (genericPaid input n >>= forceSum)
    , demanded "paid/both-abstract-leaves"
        (projectLeafSum True False . bothDemanded) (demandTuples True False . bothDemanded) paidViews
    , demanded "paid/both-fixed-leaves"
        (projectLeafSum False True . bothDemanded) (demandTuples False True . bothDemanded) paidViews
    ]

generic :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (M.PairT a Int)) (M.MaybeT (M.PairT a Int)))
generic = oneM

sourceSum :: (Eq a, Num a) => a -> Int -> a
sourceSum input n = sourceViewsSum (Source.one input n)

sourceViewsSum :: Num a => (Maybe (a, Int), Maybe (a, Int)) -> a
sourceViewsSum (first, second) = payloadSum first + payloadSum second
  where
    payloadSum Nothing = 0
    payloadSum (Just (first, second)) = first + fromIntegral second

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

selectedViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (M.PairT Int Int)) (M.MaybeT (M.PairT Int Int)))
selectedViews _ = do
    input <- M.lazy (pure (undefined :: Int))
    n <- M.lazy (pure (undefined :: Int))
    generic input n

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

fieldNames :: M.PairT (M.MaybeT (M.PairT a Int)) (M.MaybeT (M.PairT a Int)) -> M.MemoristM (Maybe ([M.ThName], [M.ThName]))
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
    n <- M.lazy (pure (undefined :: Int))
    firstNames <- generic input n >>= fieldNames
    secondNames <- generic input n >>= fieldNames
    pure (case (firstNames, secondNames) of
        (Just (first@[_, _, left, right], firstAlias),
         Just (second@[_, _, nextLeft, nextRight], secondAlias)) ->
            first == firstAlias && second == secondAlias &&
            left /= right && nextLeft /= nextRight && all (`notElem` second) first
        _ -> False)

genericBrokenTuple :: (Eq a, Num a, Scalar a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (M.PairT a Int)) (M.MaybeT (M.PairT a Int)))
genericBrokenTuple = brokenTupleM

genericBrokenAbstract :: (Eq a, Num a, Scalar a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (M.PairT a Int)) (M.MaybeT (M.PairT a Int)))
genericBrokenAbstract = brokenAbstractM

genericBrokenFixed :: (Eq a, Num a, Scalar a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (M.PairT a Int)) (M.MaybeT (M.PairT a Int)))
genericBrokenFixed = brokenFixedM

genericPaid :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (M.PairT a Int)) (M.MaybeT (M.PairT a Int)))
genericPaid = paidM

bothDemanded :: Sample -> Sample
bothDemanded sample = sample {leftDemand = True, rightDemand = True}

brokenTupleViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (M.PairT Int Int)) (M.MaybeT (M.PairT Int Int)))
brokenTupleViews _ = do
    input <- M.lazy (pure (undefined :: Int))
    genericBrokenTuple input

brokenAbstractViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (M.PairT Int Int)) (M.MaybeT (M.PairT Int Int)))
brokenAbstractViews _ = do
    input <- M.lazy (pure (undefined :: Int))
    genericBrokenAbstract input

brokenFixedViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (M.PairT Int Int)) (M.MaybeT (M.PairT Int Int)))
brokenFixedViews _ = do
    input <- M.lazy (pure (undefined :: Int))
    genericBrokenFixed input

paidViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (M.PairT Int Int)) (M.MaybeT (M.PairT Int Int)))
paidViews _ = do
    input <- M.lazy (pure (undefined :: Int))
    n <- M.lazy (pure (undefined :: Int))
    genericPaid input n

paidSourceSum :: (Eq a, Num a) => a -> Int -> a
paidSourceSum input n = sourceViewsSum (Source.paid input n)

projectLeafSum :: Bool -> Bool -> Sample -> M.PairT (M.MaybeT (M.PairT Int Int)) (M.MaybeT (M.PairT Int Int)) -> Int
projectLeafSum wantLeft wantRight sample views = case projectTuples wantLeft wantRight sample views of
    (first, second) -> payloadSum first + payloadSum second
  where
    payloadSum (Just (Just (left, right))) = fromMaybe 0 left + fromMaybe 0 right
    payloadSum _ = 0

forceTupleRoots :: M.PairT (M.MaybeT (M.PairT Int Int)) (M.MaybeT (M.PairT Int Int)) -> M.MemoristM Int
forceTupleRoots (M.PairT first second) = do
    left <- M.force first >>= tupleRoot
    right <- M.force second >>= tupleRoot
    pure (left + right)
  where
    tupleRoot M.NothingT = pure 0
    tupleRoot (M.JustT tuple) = do
        value <- M.force tuple
        pure (case value of
            M.PairT _ _ -> 1)

forceLeafSum :: Bool -> M.PairT (M.MaybeT (M.PairT Int Int)) (M.MaybeT (M.PairT Int Int)) -> M.MemoristM Int
forceLeafSum wantLeft (M.PairT first second) = do
    left <- M.force first >>= leafValue
    right <- M.force second >>= leafValue
    pure (left + right)
  where
    leafValue M.NothingT = pure 0
    leafValue (M.JustT tuple) = do
        M.PairT left right <- M.force tuple
        M.force (if wantLeft then left else right)
