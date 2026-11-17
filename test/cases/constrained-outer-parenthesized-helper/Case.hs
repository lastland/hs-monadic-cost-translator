module Case (probes) where

import Data.Maybe (fromMaybe)
import qualified Example as Source
import Generated (Scalar, oneM, twiceM)
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, scalarSource, snapshot)

probes :: [Probe]
probes =
    [ scalarSource "one/int/full-unused-input-and-count"
        (const (sourceSum (Source.one (undefined :: Int) undefined))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        genericOne input n >>= forceSum
    , scalarSource "one/integer/full-unused-input-and-count"
        (const (fromInteger (sourceSum (Source.one (undefined :: Integer) undefined)))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Integer))
        n <- M.lazy (pure (undefined :: Int))
        fromInteger <$> (genericOne input n >>= forceSum)
    , scalarSource "twice/int/full-unused-input-and-count"
        (const (sourceTwiceSum (Source.twice (undefined :: Int) undefined))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        genericTwice input n >>= forceTwiceSum
    , scalarSource "twice/integer/full-unused-input-and-count"
        (const (fromInteger (sourceTwiceSum (Source.twice (undefined :: Integer) undefined)))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Integer))
        n <- M.lazy (pure (undefined :: Int))
        fromInteger <$> (genericTwice input n >>= forceTwiceSum)
    , demanded "twice/selected-helper-result-roots" projectHelpers demandHelpers selectedTwice
    , demanded "twice/both-helpers/selected-maybe-roots"
        (projectRoots False) (demandRoots False) selectedTwice
    , demanded "twice/both-helpers/selected-list-roots"
        (projectRoots True) (demandRoots True) selectedTwice
    , snapshot "twice/distinct-helper-results/shared-maybe-list-head-and-tail-identities" $ \sample -> M.value $ M.eval1 $ do
        M.PairT first second <- selectedTwice sample
        firstNames <- M.force first >>= fieldNames
        secondNames <- M.force second >>= fieldNames
        pure (M.tname first /= M.tname second && case (firstNames, secondNames) of
            (Just (firstLeft, firstRight), Just (secondLeft, secondRight)) ->
                firstLeft == firstRight && firstLeft == secondLeft && firstLeft == secondRight
            _ -> False)
    ]

genericOne :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
genericOne = oneM

genericTwice :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
genericTwice = twiceM

sourceSum :: Num a => (Maybe [a], Maybe [a]) -> a
sourceSum (first, second) = sum (fromMaybe [] first) + sum (fromMaybe [] second)

sourceTwiceSum :: Num a => ((Maybe [a], Maybe [a]), (Maybe [a], Maybe [a])) -> a
sourceTwiceSum (first, second) = sourceSum first + sourceSum second

forceSum :: Num a => M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)) -> M.MemoristM a
forceSum (M.PairT first second) = do
    left <- M.force first >>= payloadSum
    right <- M.force second >>= payloadSum
    pure (left + right)
  where
    payloadSum M.NothingT = pure 0
    payloadSum (M.JustT list) = M.force list >>= listSum
    listSum L.NilT = pure 0
    listSum (L.ConsT headThunk tailThunk) = do
        value <- M.force headThunk
        rest <- M.force tailThunk >>= listSum
        pure (value + rest)

forceTwiceSum :: Num a => M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) -> M.MemoristM a
forceTwiceSum (M.PairT first second) = do
    left <- M.force first >>= forceSum
    right <- M.force second >>= forceSum
    pure (left + right)

selectedTwice :: Sample -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))))
selectedTwice _ = do
    input <- M.lazy (pure (undefined :: Int))
    n <- M.lazy (pure (undefined :: Int))
    genericTwice input n

projectHelpers :: Sample -> M.PairT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) -> (Maybe Bool, Maybe Bool)
projectHelpers sample (M.PairT first second) =
    (project (leftDemand sample) first, project (rightDemand sample) second)
  where
    project False _ = Nothing
    project True helper = Just (case M.tval helper of M.PairT _ _ -> True)

demandHelpers :: Sample -> M.PairT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) -> M.PairD (M.PairD (M.MaybeD (L.ListD Int)) (M.MaybeD (L.ListD Int))) (M.PairD (M.MaybeD (L.ListD Int)) (M.MaybeD (L.ListD Int)))
demandHelpers sample _ = M.PairD (demand (leftDemand sample)) (demand (rightDemand sample))
  where
    demand False = M.Skip
    demand True = M.Need (M.PairD M.Skip M.Skip)

projectRoots :: Bool -> Sample -> M.PairT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) -> ((Maybe (Maybe Bool), Maybe (Maybe Bool)), (Maybe (Maybe Bool), Maybe (Maybe Bool)))
projectRoots wantLists sample (M.PairT first second) =
    (views (M.tval first), views (M.tval second))
  where
    views (M.PairT left right) =
        (project (leftDemand sample) left, project (rightDemand sample) right)
    project False _ = Nothing
    project True root = Just (case M.tval root of
        M.NothingT -> Nothing
        M.JustT list -> Just (not wantLists || case M.tval list of
            L.NilT -> False
            L.ConsT _ _ -> True))

demandRoots :: Bool -> Sample -> M.PairT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) -> M.PairD (M.PairD (M.MaybeD (L.ListD Int)) (M.MaybeD (L.ListD Int))) (M.PairD (M.MaybeD (L.ListD Int)) (M.MaybeD (L.ListD Int)))
demandRoots wantLists sample (M.PairT first second) = M.PairD
    (M.Need (views (M.tval first))) (M.Need (views (M.tval second)))
  where
    views (M.PairT left right) = M.PairD
        (demand (leftDemand sample) left) (demand (rightDemand sample) right)
    demand False _ = M.Skip
    demand True root = M.Need (case M.tval root of
        M.NothingT -> M.NothingD
        M.JustT list -> M.JustD (if wantLists then M.Need (case M.tval list of
            L.NilT -> L.NilD
            L.ConsT _ _ -> L.ConsD M.Skip M.Skip) else M.Skip))

fieldNames :: M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)) -> M.MemoristM (Maybe ([M.ThName], [M.ThName]))
fieldNames (M.PairT first second) = do
    left <- M.force first
    right <- M.force second
    case (left, right) of
        (M.JustT leftList, M.JustT rightList) -> do
            leftValue <- M.force leftList
            rightValue <- M.force rightList
            pure (case (leftValue, rightValue) of
                (L.ConsT leftHead leftTail, L.ConsT rightHead rightTail) -> Just
                    ( [M.tname first, M.tname leftList, M.tname leftHead, M.tname leftTail]
                    , [M.tname second, M.tname rightList, M.tname rightHead, M.tname rightTail]
                    )
                _ -> Nothing)
        _ -> pure Nothing
