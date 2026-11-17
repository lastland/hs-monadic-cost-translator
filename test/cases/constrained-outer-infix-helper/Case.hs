module Case (probes) where

import Control.Monad ((>=>), replicateM)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import qualified Example as Source
import Generated (Scalar, oneM, twiceM, cachedHelperM, separateM, brokenHeadM, brokenRootM, symbolicM, mixedM)
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, failure, scalarSource, snapshot)

probes :: [Probe]
probes =
    [ scalarSource "one/int/full-unused-count"
        (const (sourceSum (Source.one (undefined :: Int) undefined))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        genericOne input n >>= forceSum
    , scalarSource "one/integer/full-unused-count"
        (const (fromInteger (sourceSum (Source.one (undefined :: Integer) undefined)))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Integer))
        n <- M.lazy (pure (undefined :: Int))
        fromInteger <$> (genericOne input n >>= forceSum)
    , scalarSource "twice/int/full-unused-count"
        (const (sourceTwiceSum (Source.twice (undefined :: Int) undefined))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        genericTwice input n >>= forceTwiceSum
    , scalarSource "twice/integer/full-unused-count"
        (const (fromInteger (sourceTwiceSum (Source.twice (undefined :: Integer) undefined)))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Integer))
        n <- M.lazy (pure (undefined :: Int))
        fromInteger <$> (genericTwice input n >>= forceTwiceSum)
    , demanded "twice/selected-helper-roots" projectHelpers demandHelpers selectedTwice
    , demanded "twice/both-helpers/selected-maybe-roots"
        (projectRoots False) (demandRoots False) selectedTwice
    , demanded "twice/both-helpers/selected-list-roots"
        (projectRoots True) (demandRoots True) selectedTwice
    , snapshot "twice/shared-maybe-list-head-and-tail-identities" $ \sample -> M.value $ M.eval1 $ do
        M.PairT first second <- selectedTwice sample
        firstNames <- M.force first >>= fieldNames
        secondNames <- M.force second >>= fieldNames
        pure (M.tname first /= M.tname second && case (firstNames, secondNames) of
            (Just (firstLeft, firstRight), Just (secondLeft, secondRight)) ->
                firstLeft == firstRight && firstLeft == secondLeft && firstLeft == secondRight
            _ -> False)
    ]
    ++ preservationProbes

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

preservationProbes :: [Probe]
preservationProbes =
    [ scalarSource "one/repeated-fresh-outer-calls" sourceRepeatedOne $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        sum <$> replicateM (repeats sample) (genericOne input n >>= forceSum)
    , scalarSource "one/repeated-cached-outer-call" sourceRepeatedOne $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        cached <- M.lazy (genericOne input n)
        sum <$> replicateM (repeats sample) (M.force cached >>= forceSum)
    , scalarSource "twice/repeated-fresh-outer-calls" sourceRepeatedTwice $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        sum <$> replicateM (repeats sample) (genericTwice input n >>= forceTwiceSum)
    , scalarSource "twice/repeated-cached-outer-call" sourceRepeatedTwice $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        cached <- M.lazy (genericTwice input n)
        sum <$> replicateM (repeats sample) (M.force cached >>= forceTwiceSum)
    , snapshot "twice/fresh-outer-identities" $ \_ -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        first@(M.PairT firstLeft firstRight) <- genericTwice input n
        second@(M.PairT secondLeft secondRight) <- genericTwice input n
        firstNames <- twiceFieldNames first
        secondNames <- twiceFieldNames second
        pure (length (nub (map M.tname [firstLeft, firstRight, secondLeft, secondRight])) == 4 &&
            case (firstNames, secondNames) of
                (Just ((a, b), (c, d)), Just ((e, f), (g, h))) ->
                    a == b && a == c && a == d && e == f && e == g && e == h && all (`notElem` e) a
                _ -> False)
    , scalarSource "cached-helper/int/full"
        (const (sourceTwiceSum (Source.cachedHelper (undefined :: Int) undefined))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        genericCachedHelper input n >>= forceTwiceSum
    , snapshot "cached-helper/shared-result-and-fields" $ \_ -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        result@(M.PairT first second) <- genericCachedHelper input n
        names <- twiceFieldNames result
        pure (M.tname first == M.tname second && case names of
            Just ((a, b), (c, d)) -> a == b && a == c && a == d
            _ -> False)
    , scalarSource "separate/int/full"
        (const (sourceTwiceSum (Source.separate (undefined :: Int) undefined))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        genericSeparate input n >>= forceTwiceSum
    , snapshot "separate/distinct-bindings-shared-across-helpers" $ \_ -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        result@(M.PairT first second) <- genericSeparate input n
        names <- twiceFieldNames result
        pure (M.tname first /= M.tname second && case names of
            Just ((firstLeft, firstRight), (secondLeft, secondRight)) ->
                firstLeft == secondLeft && firstRight == secondRight && all (`notElem` firstRight) firstLeft
            _ -> False)
    , demanded "broken-head/both-list-spines"
        (projectRoots True . bothDemanded) demandSpines selectedBrokenHead
    , failure "broken-head/full" (selectedBrokenHead >=> forceTwiceSum)
    , demanded "broken-root/both-helper-roots"
        (projectHelpers . bothDemanded) (demandHelpers . bothDemanded) selectedBrokenRoot
    , failure "broken-root/full" (selectedBrokenRoot >=> forceTwiceSum)
    , scalarSource "symbolic/int/full"
        (const (sourceSum (Source.symbolic (undefined :: Int) undefined))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        genericSymbolic input n >>= forceSum
    , snapshot "symbolic/shared-maybe-list-head-and-tail-identities" $ \_ -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        names <- genericSymbolic input n >>= fieldNames
        pure (case names of
            Just (first, second) -> first == second
            _ -> False)
    , scalarSource "mixed/int/full"
        (const (sourceTwiceSum (Source.mixed (undefined :: Int) undefined))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        genericMixed input n >>= forceTwiceSum
    , snapshot "mixed/distinct-helpers-and-shared-fields" $ \_ -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        result@(M.PairT first second) <- genericMixed input n
        names <- twiceFieldNames result
        pure (M.tname first /= M.tname second && case names of
            Just ((a, b), (c, d)) -> a == b && a == c && a == d
            _ -> False)
    ]

sourceRepeatedOne :: Sample -> Int
sourceRepeatedOne sample = repeats sample * sourceSum (Source.one (undefined :: Int) undefined)

sourceRepeatedTwice :: Sample -> Int
sourceRepeatedTwice sample = repeats sample * sourceTwiceSum (Source.twice (undefined :: Int) undefined)

genericCachedHelper :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
genericCachedHelper = cachedHelperM

genericSeparate :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
genericSeparate = separateM

genericBrokenHead :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
genericBrokenHead = brokenHeadM

genericBrokenRoot :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
genericBrokenRoot = brokenRootM

genericSymbolic :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
genericSymbolic = symbolicM

genericMixed :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
genericMixed = mixedM

twiceFieldNames :: M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) -> M.MemoristM (Maybe (([M.ThName], [M.ThName]), ([M.ThName], [M.ThName])))
twiceFieldNames (M.PairT first second) = do
    left <- M.force first >>= fieldNames
    right <- M.force second >>= fieldNames
    pure ((,) <$> left <*> right)

selectedBrokenHead :: Sample -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))))
selectedBrokenHead _ = do
    input <- M.lazy (pure (undefined :: Int))
    n <- M.lazy (pure (undefined :: Int))
    genericBrokenHead input n

selectedBrokenRoot :: Sample -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))))
selectedBrokenRoot _ = do
    input <- M.lazy (pure (undefined :: Int))
    n <- M.lazy (pure (undefined :: Int))
    genericBrokenRoot input n

bothDemanded :: Sample -> Sample
bothDemanded sample = sample {leftDemand = True, rightDemand = True}

demandSpines :: Sample -> M.PairT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) -> M.PairD (M.PairD (M.MaybeD (L.ListD Int)) (M.MaybeD (L.ListD Int))) (M.PairD (M.MaybeD (L.ListD Int)) (M.MaybeD (L.ListD Int)))
demandSpines _ (M.PairT first second) = M.PairD
    (M.Need (views (M.tval first))) (M.Need (views (M.tval second)))
  where
    views (M.PairT left right) = M.PairD
        (M.Need (payload (M.tval left))) (M.Need (payload (M.tval right)))
    payload M.NothingT = M.NothingD
    payload (M.JustT list) = M.JustD (M.Need (spine (M.tval list)))
    spine L.NilT = L.NilD
    spine (L.ConsT _ tailThunk) = L.ConsD M.Skip (M.Need (spine (M.tval tailThunk)))
