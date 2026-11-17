module Case (probes) where

import Control.Monad ((>=>), replicateM)
import Data.Maybe (fromMaybe)
import qualified Example as Source
import Generated (Scalar, oneM, brokenInnerM, brokenLeafM)
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, failure, scalarSource, snapshot)

probes :: [Probe]
probes =
    [ scalarSource "just-just/int/full"
        (sourceSum (undefined :: Int) . positiveCount) $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (positiveCount sample))
        generic input n >>= forceSum
    , scalarSource "just-just/integer/full"
        (fromInteger . sourceSum (undefined :: Integer) . positiveCount) $ \sample -> do
        input <- M.lazy (pure (undefined :: Integer))
        n <- M.lazy (pure (positiveCount sample))
        fromInteger <$> (generic input n >>= forceSum)
    , scalarSource "just-nothing/full"
        (const (sourceSum (undefined :: Int) 0)) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure 0)
        generic input n >>= forceSum
    , demanded "selected/payloads" projectPayloads demandPayloads selectedViews
    , demanded "selected/outer-roots" projectOuterRoots demandOuterRoots selectedViews
    , demanded "selected/inner-roots" projectInnerRoots demandInnerRoots selectedViews
    , snapshot "just-just/outer-inner-and-leaf-aliases" $ \sample -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (positiveCount sample))
        M.PairT first second <- generic input n
        left <- M.force first
        right <- M.force second
        case (left, right) of
            (M.JustT leftInner, M.JustT rightInner) -> do
                leftValue <- M.force leftInner
                rightValue <- M.force rightInner
                pure (M.tname first == M.tname second &&
                    M.tname leftInner == M.tname rightInner &&
                    case (leftValue, rightValue) of
                        (M.JustT leftLeaf, M.JustT rightLeaf) -> M.tname leftLeaf == M.tname rightLeaf
                        _ -> False)
            _ -> pure False
    , scalarSource "just-just/repeated-fresh-calls"
        (\sample -> repeats sample * sourceSum (undefined :: Int) (positiveCount sample)) $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (positiveCount sample))
        sum <$> replicateM (repeats sample) (generic input n >>= forceSum)
    , scalarSource "just-just/repeated-cached-call"
        (\sample -> repeats sample * sourceSum (undefined :: Int) (positiveCount sample)) $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (positiveCount sample))
        cached <- M.lazy (generic input n)
        sum <$> replicateM (repeats sample) (M.force cached >>= forceSum)
    , snapshot "just-just/fresh-outer-inner-and-leaf-identities" $ \sample -> M.value $ M.eval1 $ do
        let names (M.PairT first second) = do
                left <- M.force first
                right <- M.force second
                case (left, right) of
                    (M.JustT leftInner, M.JustT rightInner) -> do
                        leftValue <- M.force leftInner
                        rightValue <- M.force rightInner
                        pure (case (leftValue, rightValue) of
                            (M.JustT leftLeaf, M.JustT rightLeaf) -> Just
                                ( [M.tname first, M.tname leftInner, M.tname leftLeaf]
                                , [M.tname second, M.tname rightInner, M.tname rightLeaf]
                                )
                            _ -> Nothing)
                    _ -> pure Nothing
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (positiveCount sample))
        firstNames <- generic input n >>= names
        secondNames <- generic input n >>= names
        pure (case (firstNames, secondNames) of
            (Just (first, firstAlias), Just (second, secondAlias)) ->
                first == firstAlias && second == secondAlias && all (`notElem` second) first
            _ -> False)
    , demanded "undefined-count/skipped" (\_ _ -> ())
        (\_ _ -> M.PairD M.Skip M.Skip) undefinedCountViews
    , demanded "undefined-count/both-outer-roots"
        (projectOuterRoots . bothDemanded) (demandOuterRoots . bothDemanded) undefinedCountViews
    , failure "undefined-count/inner-roots" (undefinedCountViews >=> forceInnerRoots)
    , failure "undefined-count/full" (undefinedCountViews >=> forceSum)
    , demanded "broken-inner/both-outer-roots"
        (projectOuterRoots . bothDemanded) (demandOuterRoots . bothDemanded) brokenInnerViews
    , failure "broken-inner/inner-roots" (brokenInnerViews >=> forceInnerRoots)
    , failure "broken-inner/full" (brokenInnerViews >=> forceSum)
    , demanded "broken-leaf/both-inner-roots"
        (projectInnerRoots . bothDemanded) (demandInnerRoots . bothDemanded) brokenLeafViews
    , failure "broken-leaf/payload" (brokenLeafViews >=> forceSum)
    ]

generic :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (M.MaybeT a)) (M.MaybeT (M.MaybeT a)))
generic = oneM

positiveCount :: Sample -> Int
positiveCount sample = max 1 (number sample)

sourceSum :: (Eq a, Num a) => a -> Int -> a
sourceSum input n = case Source.one input n of
    (first, second) -> payloadValue first + payloadValue second
  where
    payloadValue = fromMaybe 0 . fromMaybe Nothing

forceSum :: Num a => M.PairT (M.MaybeT (M.MaybeT a)) (M.MaybeT (M.MaybeT a)) -> M.MemoristM a
forceSum (M.PairT first second) = do
    left <- M.force first >>= innerValue
    right <- M.force second >>= innerValue
    pure (left + right)
  where
    innerValue M.NothingT = pure 0
    innerValue (M.JustT inner) = M.force inner >>= payloadValue
    payloadValue M.NothingT = pure 0
    payloadValue (M.JustT payload) = M.force payload

selectedViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (M.MaybeT Int)) (M.MaybeT (M.MaybeT Int)))
selectedViews sample = do
    input <- M.lazy (pure (undefined :: Int))
    n <- M.lazy (pure (if even (variant sample) then 0 else positiveCount sample))
    generic input n

projectPayloads :: Sample -> M.PairT (M.MaybeT (M.MaybeT Int)) (M.MaybeT (M.MaybeT Int)) -> (Maybe (Maybe (Maybe Int)), Maybe (Maybe (Maybe Int)))
projectPayloads sample (M.PairT first second) =
    (project (leftDemand sample) first, project (rightDemand sample) second)
  where
    project False _ = Nothing
    project True root = Just (case M.tval root of
        M.NothingT -> Nothing
        M.JustT inner -> Just (case M.tval inner of
            M.NothingT -> Nothing
            M.JustT payload -> Just (M.tval payload)))

demandPayloads :: Sample -> M.PairT (M.MaybeT (M.MaybeT Int)) (M.MaybeT (M.MaybeT Int)) -> M.PairD (M.MaybeD (M.MaybeD Int)) (M.MaybeD (M.MaybeD Int))
demandPayloads sample (M.PairT first second) = M.PairD
    (if leftDemand sample then M.fullDemand first else M.Skip)
    (if rightDemand sample then M.fullDemand second else M.Skip)

projectOuterRoots :: Sample -> M.PairT (M.MaybeT (M.MaybeT Int)) (M.MaybeT (M.MaybeT Int)) -> (Maybe Bool, Maybe Bool)
projectOuterRoots sample (M.PairT first second) =
    (project (leftDemand sample) first, project (rightDemand sample) second)
  where
    project False _ = Nothing
    project True root = Just (case M.tval root of
        M.NothingT -> False
        M.JustT _ -> True)

demandOuterRoots :: Sample -> M.PairT (M.MaybeT (M.MaybeT Int)) (M.MaybeT (M.MaybeT Int)) -> M.PairD (M.MaybeD (M.MaybeD Int)) (M.MaybeD (M.MaybeD Int))
demandOuterRoots sample (M.PairT first second) = M.PairD
    (demand (leftDemand sample) first) (demand (rightDemand sample) second)
  where
    demand False _ = M.Skip
    demand True root = M.Need (case M.tval root of
        M.NothingT -> M.NothingD
        M.JustT _ -> M.JustD M.Skip)

projectInnerRoots :: Sample -> M.PairT (M.MaybeT (M.MaybeT Int)) (M.MaybeT (M.MaybeT Int)) -> (Maybe (Maybe Bool), Maybe (Maybe Bool))
projectInnerRoots sample (M.PairT first second) =
    (project (leftDemand sample) first, project (rightDemand sample) second)
  where
    project False _ = Nothing
    project True root = Just (case M.tval root of
        M.NothingT -> Nothing
        M.JustT inner -> Just (case M.tval inner of
            M.NothingT -> False
            M.JustT _ -> True))

demandInnerRoots :: Sample -> M.PairT (M.MaybeT (M.MaybeT Int)) (M.MaybeT (M.MaybeT Int)) -> M.PairD (M.MaybeD (M.MaybeD Int)) (M.MaybeD (M.MaybeD Int))
demandInnerRoots sample (M.PairT first second) = M.PairD
    (demand (leftDemand sample) first) (demand (rightDemand sample) second)
  where
    demand False _ = M.Skip
    demand True root = M.Need (case M.tval root of
        M.NothingT -> M.NothingD
        M.JustT inner -> M.JustD (M.Need (case M.tval inner of
            M.NothingT -> M.NothingD
            M.JustT _ -> M.JustD M.Skip)))

genericBrokenInner :: (Eq a, Num a, Scalar a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (M.MaybeT a)) (M.MaybeT (M.MaybeT a)))
genericBrokenInner = brokenInnerM

genericBrokenLeaf :: (Eq a, Num a, Scalar a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (M.MaybeT a)) (M.MaybeT (M.MaybeT a)))
genericBrokenLeaf = brokenLeafM

bothDemanded :: Sample -> Sample
bothDemanded sample = sample {leftDemand = True, rightDemand = True}

undefinedCountViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (M.MaybeT Int)) (M.MaybeT (M.MaybeT Int)))
undefinedCountViews _ = do
    input <- M.lazy (pure (undefined :: Int))
    n <- M.lazy (pure (undefined :: Int))
    generic input n

brokenInnerViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (M.MaybeT Int)) (M.MaybeT (M.MaybeT Int)))
brokenInnerViews _ = do
    input <- M.lazy (pure (undefined :: Int))
    genericBrokenInner input

brokenLeafViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (M.MaybeT Int)) (M.MaybeT (M.MaybeT Int)))
brokenLeafViews _ = do
    input <- M.lazy (pure (undefined :: Int))
    genericBrokenLeaf input

forceInnerRoots :: M.PairT (M.MaybeT (M.MaybeT a)) (M.MaybeT (M.MaybeT a)) -> M.MemoristM Int
forceInnerRoots (M.PairT first second) = do
    left <- M.force first >>= innerRoot
    right <- M.force second >>= innerRoot
    pure (left + right)
  where
    innerRoot M.NothingT = pure 0
    innerRoot (M.JustT inner) = do
        value <- M.force inner
        pure (case value of
            M.NothingT -> 0
            M.JustT _ -> 1)
