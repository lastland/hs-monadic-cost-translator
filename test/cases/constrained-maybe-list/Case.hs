module Case (probes) where

import Control.Monad ((>=>), replicateM)
import Data.Maybe (fromMaybe)
import qualified Example as Source
import Generated (Scalar, oneM, viewsM, brokenHeadM, brokenTailM)
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, failure, scalarSource, snapshot)

probes :: [Probe]
probes =
    [ scalarSource "just-list/int/full-unused-count"
        (const (sourceSum (undefined :: Int) undefined)) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        generic input n >>= forceSum
    , scalarSource "just-list/integer/full-unused-count"
        (const (fromInteger (sourceSum (undefined :: Integer) undefined))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Integer))
        n <- M.lazy (pure (undefined :: Int))
        fromInteger <$> (generic input n >>= forceSum)
    , demanded "selected/maybe-roots" projectRoots demandRoots selectedViews
    , demanded "selected/list-prefix-one"
        (projectLists False False) (demandLists False False) selectedViews
    , demanded "selected/list-spines-through-nil"
        (projectLists False True) (demandLists False True) selectedViews
    , demanded "selected/heads-without-tails"
        (projectLists True False) (demandLists True False) selectedViews
    , snapshot "just-list/maybe-list-head-and-tail-aliases" $ \sample -> M.value $ M.eval1 $ do
        names <- selectedViews sample >>= fieldNames
        pure (case names of
            Just (first, second) -> first == second
            Nothing -> False)
    , scalarSource "just-list/repeated-fresh-calls"
        (\sample -> repeats sample * sourceSum (undefined :: Int) undefined) $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        sum <$> replicateM (repeats sample) (generic input n >>= forceSum)
    , scalarSource "just-list/repeated-cached-call"
        (\sample -> repeats sample * sourceSum (undefined :: Int) undefined) $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (undefined :: Int))
        cached <- M.lazy (generic input n)
        sum <$> replicateM (repeats sample) (M.force cached >>= forceSum)
    , snapshot "just-list/fresh-maybe-list-head-and-tail-identities" freshIdentities
    , scalarSource "views/int/full"
        (sourceViewsSum (undefined :: Int) . positiveCount) $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure (positiveCount sample))
        genericViews input n >>= forceSum
    , scalarSource "views/integer/full"
        (fromInteger . sourceViewsSum (undefined :: Integer) . positiveCount) $ \sample -> do
        input <- M.lazy (pure (undefined :: Integer))
        n <- M.lazy (pure (positiveCount sample))
        fromInteger <$> (genericViews input n >>= forceSum)
    , scalarSource "views/empty/full"
        (const (sourceViewsSum (undefined :: Int) 0)) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        n <- M.lazy (pure 0)
        genericViews input n >>= forceSum
    , demanded "views/selected/list-prefix-one"
        (projectLists False False) (demandLists False False) selectedBuiltViews
    , demanded "views/selected/list-spines-through-nil"
        (projectLists False True) (demandLists False True) selectedBuiltViews
    , demanded "views/selected/heads-without-tails"
        (projectLists True False) (demandLists True False) selectedBuiltViews
    , demanded "views/undefined-count/both-maybe-roots"
        (projectRoots . bothDemanded) (demandRoots . bothDemanded) undefinedCountViews
    , failure "views/undefined-count/first-list-root" (undefinedCountViews >=> forceFirstListRoot)
    , failure "views/undefined-count/full" (undefinedCountViews >=> forceSum)
    , demanded "broken-head/both-spines-through-nil"
        (projectLists False True . bothDemanded) (demandLists False True . bothDemanded) brokenHeadViews
    , failure "broken-head/first-head" (brokenHeadViews >=> forceFirstHead)
    , failure "broken-head/full" (brokenHeadViews >=> forceSum)
    , demanded "broken-tail/both-list-prefixes-one"
        (projectLists False False . bothDemanded) (demandLists False False . bothDemanded) brokenTailViews
    , demanded "broken-tail/both-heads-without-tails"
        (projectLists True False . bothDemanded) (demandLists True False . bothDemanded) brokenTailViews
    , failure "broken-tail/first-tail" (brokenTailViews >=> forceFirstTail)
    , failure "broken-tail/all-spines" (brokenTailViews >=> forceSpineSum)
    , failure "broken-tail/full" (brokenTailViews >=> forceSum)
    ]

generic :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
generic = oneM

sourceSum :: (Eq a, Num a) => a -> Int -> a
sourceSum input n = case Source.one input n of
    (first, second) -> sum (fromMaybe [] first) + sum (fromMaybe [] second)

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

selectedViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)))
selectedViews _ = do
    input <- M.lazy (pure (undefined :: Int))
    n <- M.lazy (pure (undefined :: Int))
    generic input n

projectRoots :: Sample -> M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)) -> (Maybe Bool, Maybe Bool)
projectRoots sample (M.PairT first second) =
    (project (leftDemand sample) first, project (rightDemand sample) second)
  where
    project False _ = Nothing
    project True root = Just (case M.tval root of
        M.NothingT -> False
        M.JustT _ -> True)

demandRoots :: Sample -> M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)) -> M.PairD (M.MaybeD (L.ListD Int)) (M.MaybeD (L.ListD Int))
demandRoots sample (M.PairT first second) = M.PairD
    (demand (leftDemand sample) first) (demand (rightDemand sample) second)
  where
    demand False _ = M.Skip
    demand True root = M.Need (case M.tval root of
        M.NothingT -> M.NothingD
        M.JustT _ -> M.JustD M.Skip)

projectLists :: Bool -> Bool -> Sample -> M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)) -> (Maybe (Maybe [Maybe Int]), Maybe (Maybe [Maybe Int]))
projectLists wantHeads wantTails sample (M.PairT first second) =
    (project (leftDemand sample) first, project (rightDemand sample) second)
  where
    project False _ = Nothing
    project True root = Just (case M.tval root of
        M.NothingT -> Nothing
        M.JustT list -> Just (listView (M.tval list)))
    listView L.NilT = []
    listView (L.ConsT headThunk tailThunk) =
        (if wantHeads then Just (M.tval headThunk) else Nothing) :
        (if wantTails then listView (M.tval tailThunk) else [])

demandLists :: Bool -> Bool -> Sample -> M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)) -> M.PairD (M.MaybeD (L.ListD Int)) (M.MaybeD (L.ListD Int))
demandLists wantHeads wantTails sample (M.PairT first second) = M.PairD
    (demand (leftDemand sample) first) (demand (rightDemand sample) second)
  where
    demand False _ = M.Skip
    demand True root = M.Need (case M.tval root of
        M.NothingT -> M.NothingD
        M.JustT list -> M.JustD (M.Need (listDemand (M.tval list))))
    listDemand L.NilT = L.NilD
    listDemand (L.ConsT headThunk tailThunk) = L.ConsD
        (if wantHeads then M.Need (M.tval headThunk) else M.Skip)
        (if wantTails then M.Need (listDemand (M.tval tailThunk)) else M.Skip)

freshIdentities :: Sample -> Bool
freshIdentities _ = M.value $ M.eval1 $ do
    input <- M.lazy (pure (undefined :: Int))
    n <- M.lazy (pure (undefined :: Int))
    firstNames <- generic input n >>= fieldNames
    secondNames <- generic input n >>= fieldNames
    pure (case (firstNames, secondNames) of
        (Just (first, firstAlias), Just (second, secondAlias)) ->
            first == firstAlias && second == secondAlias && all (`notElem` second) first
        _ -> False)

fieldNames :: M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)) -> M.MemoristM (Maybe ([M.ThName], [M.ThName]))
fieldNames (M.PairT first second) = do
    left <- M.force first
    right <- M.force second
    case (left, right) of
        (M.JustT leftList, M.JustT rightList) -> do
            leftValue <- M.force leftList
            rightValue <- M.force rightList
            case (leftValue, rightValue) of
                (L.ConsT leftHead leftTail, L.ConsT rightHead rightTail) -> do
                    leftEnd <- M.force leftTail
                    rightEnd <- M.force rightTail
                    pure (case (leftEnd, rightEnd) of
                        (L.NilT, L.NilT) -> Just
                            ( [M.tname first, M.tname leftList, M.tname leftHead, M.tname leftTail]
                            , [M.tname second, M.tname rightList, M.tname rightHead, M.tname rightTail]
                            )
                        _ -> Nothing)
                _ -> pure Nothing
        _ -> pure Nothing

genericViews :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
genericViews = viewsM

positiveCount :: Sample -> Int
positiveCount sample = width sample + 2

sourceViewsSum :: (Eq a, Num a) => a -> Int -> a
sourceViewsSum input n = case Source.views input n of
    (first, second) -> sum (fromMaybe [] first) + sum (fromMaybe [] second)

selectedBuiltViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)))
selectedBuiltViews sample = do
    input <- M.lazy (pure (undefined :: Int))
    n <- M.lazy (pure (positiveCount sample))
    genericViews input n

bothDemanded :: Sample -> Sample
bothDemanded sample = sample {leftDemand = True, rightDemand = True}

undefinedCountViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)))
undefinedCountViews _ = do
    input <- M.lazy (pure (undefined :: Int))
    n <- M.lazy (pure (undefined :: Int))
    genericViews input n

genericBrokenHead :: (Eq a, Num a, Scalar a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
genericBrokenHead = brokenHeadM

genericBrokenTail :: (Eq a, Num a, Scalar a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))
genericBrokenTail = brokenTailM

brokenHeadViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)))
brokenHeadViews _ = do
    input <- M.lazy (pure (undefined :: Int))
    genericBrokenHead input

brokenTailViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)))
brokenTailViews _ = do
    input <- M.lazy (pure (undefined :: Int))
    genericBrokenTail input

forceFirstListRoot :: M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)) -> M.MemoristM Int
forceFirstListRoot (M.PairT first _) = do
    value <- M.force first
    case value of
        M.NothingT -> pure 0
        M.JustT list -> do
            xs <- M.force list
            pure (case xs of
                L.NilT -> 0
                L.ConsT _ _ -> 1)

forceFirstHead :: M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)) -> M.MemoristM Int
forceFirstHead (M.PairT first _) = do
    value <- M.force first
    case value of
        M.NothingT -> pure 0
        M.JustT list -> do
            xs <- M.force list
            case xs of
                L.NilT -> pure 0
                L.ConsT headThunk _ -> M.force headThunk

forceFirstTail :: M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)) -> M.MemoristM Int
forceFirstTail (M.PairT first _) = do
    value <- M.force first
    case value of
        M.NothingT -> pure 0
        M.JustT list -> do
            xs <- M.force list
            case xs of
                L.NilT -> pure 0
                L.ConsT _ tailThunk -> do
                    rest <- M.force tailThunk
                    pure (case rest of
                        L.NilT -> 0
                        L.ConsT _ _ -> 1)

forceSpineSum :: M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)) -> M.MemoristM Int
forceSpineSum (M.PairT first second) = do
    left <- M.force first >>= payloadLength
    right <- M.force second >>= payloadLength
    pure (left + right)
  where
    payloadLength M.NothingT = pure 0
    payloadLength (M.JustT list) = M.force list >>= listLength
    listLength L.NilT = pure 0
    listLength (L.ConsT _ tailThunk) = do
        rest <- M.force tailThunk >>= listLength
        pure (rest + 1)
