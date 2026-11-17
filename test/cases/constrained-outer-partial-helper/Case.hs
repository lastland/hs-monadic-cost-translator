module Case (probes) where

import Control.Monad ((>=>), replicateM)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import qualified Example as Source
import Generated (Scalar, oneM, manyM, twiceM, cachedMapM, separateManyM, mixedM, brokenHeadM, brokenRootM)
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, failure, scalarSource, snapshot)

probes :: [Probe]
probes =
    [ scalarSource "one/int/full-unused-input"
        (const (sourceSum (Source.one (undefined :: Int)))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        genericOne input >>= forceSum
    , scalarSource "one/integer/full-unused-input"
        (const (fromInteger (sourceSum (Source.one (undefined :: Integer))))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Integer))
        fromInteger <$> (genericOne input >>= forceSum)
    , scalarSource "many/int/full-unused-count-elements"
        (sourceSum . Source.many (undefined :: Int) . countValues)
        (selectedMany >=> forceSum)
    , scalarSource "many/integer/full-unused-count-elements"
        (fromInteger . sourceSum . Source.many (undefined :: Integer) . countValues) $ \sample -> do
        input <- M.lazy (pure (undefined :: Integer))
        counts <- M.lazy (countList (countValues sample))
        fromInteger <$> (genericMany input counts >>= forceSum)
    , demanded "many/all-spines" (projectManyRoots False) (demandManyRoots False) selectedMany
    , demanded "many/all-helper-roots" (projectManyRoots True) (demandManyRoots True) selectedMany
    , snapshot "many/shared-fields-and-distinct-helper-results" $ \sample -> M.value $ M.eval1 $ do
        names <- selectedMany sample >>= manyFieldNames
        pure (case names of
            Nothing -> False
            Just [] -> True
            Just entries@((_, shared, _):_) ->
                all (\(_, left, right) -> left == shared && right == shared) entries &&
                let roots = map (\(root, _, _) -> root) entries
                in length roots == length (nub roots))
    , scalarSource "many/empty/full-no-helper-entry"
        (const (sourceSum (Source.many (undefined :: Int) []))) $ \sample ->
        selectedMany (sample {width = 0}) >>= forceSum
    ]
    ++ preservationProbes

genericOne :: (Eq a, Num a, Scalar a) => M.Th a -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
genericOne = oneM

genericMany :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th (L.ListT Int) -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
genericMany = manyM

sourceSum :: Num a => [(Maybe [a], Maybe [a])] -> a
sourceSum = sum . map pairSum
  where
    pairSum (first, second) = sum (fromMaybe [] first) + sum (fromMaybe [] second)

forceSum :: Num a => L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) -> M.MemoristM a
forceSum L.NilT = pure 0
forceSum (L.ConsT headThunk tailThunk) = do
    value <- M.force headThunk >>= pairSum
    rest <- M.force tailThunk >>= forceSum
    pure (value + rest)
  where
    pairSum (M.PairT first second) = do
        left <- M.force first >>= payloadSum
        right <- M.force second >>= payloadSum
        pure (left + right)
    payloadSum M.NothingT = pure 0
    payloadSum (M.JustT list) = M.force list >>= listSum
    listSum L.NilT = pure 0
    listSum (L.ConsT first rest) = do
        value <- M.force first
        remaining <- M.force rest >>= listSum
        pure (value + remaining)

countValues :: Sample -> [Int]
countValues sample = replicate (max 0 (width sample)) (undefined :: Int)

countList :: [Int] -> M.MemoristM (L.ListT Int)
countList [] = pure L.NilT
countList (first:rest) = do
    headThunk <- M.lazy (pure first)
    tailThunk <- M.lazy (countList rest)
    pure (L.ConsT headThunk tailThunk)

selectedMany :: Sample -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))))
selectedMany sample = do
    input <- M.lazy (pure (undefined :: Int))
    counts <- M.lazy (countList (countValues sample))
    genericMany input counts

projectManyRoots :: Bool -> Sample -> L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) -> [Maybe Bool]
projectManyRoots wantHelpers _ = walk
  where
    walk L.NilT = []
    walk (L.ConsT headThunk tailThunk) =
        (if wantHelpers then Just (case M.tval headThunk of M.PairT _ _ -> True) else Nothing) :
        walk (M.tval tailThunk)

demandManyRoots :: Bool -> Sample -> L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) -> L.ListD (M.PairD (M.MaybeD (L.ListD Int)) (M.MaybeD (L.ListD Int)))
demandManyRoots wantHelpers _ = walk
  where
    walk L.NilT = L.NilD
    walk (L.ConsT _ tailThunk) = L.ConsD
        (if wantHelpers then M.Need (M.PairD M.Skip M.Skip) else M.Skip)
        (M.Need (walk (M.tval tailThunk)))

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

manyFieldNames :: L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) -> M.MemoristM (Maybe [(M.ThName, [M.ThName], [M.ThName])])
manyFieldNames L.NilT = pure (Just [])
manyFieldNames (L.ConsT headThunk tailThunk) = do
    names <- M.force headThunk >>= fieldNames
    rest <- M.force tailThunk >>= manyFieldNames
    pure (do
        (left, right) <- names
        remaining <- rest
        pure ((M.tname headThunk, left, right) : remaining))

preservationProbes :: [Probe]
preservationProbes =
    [ scalarSource "many/repeated-fresh-outer-calls" sourceRepeatedMany $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        counts <- M.lazy (countList (countValues sample))
        sum <$> replicateM (max 1 (repeats sample)) (genericMany input counts >>= forceSum)
    , scalarSource "many/repeated-cached-outer-call" sourceRepeatedMany $ \sample -> do
        input <- M.lazy (pure (undefined :: Int))
        counts <- M.lazy (countList (countValues sample))
        cached <- M.lazy (genericMany input counts)
        sum <$> replicateM (max 1 (repeats sample)) (M.force cached >>= forceSum)
    , snapshot "many/fresh-outer-identities" $ \sample -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        counts <- M.lazy (countList (countValues sample))
        firstNames <- genericMany input counts >>= manyFieldNames
        secondNames <- genericMany input counts >>= manyFieldNames
        pure (case (firstNames, secondNames) of
            (Just firstEntries, Just secondEntries) ->
                sameSharedFields firstEntries && sameSharedFields secondEntries &&
                distinctHelperResults (firstEntries ++ secondEntries) &&
                case (firstEntries, secondEntries) of
                    ([], []) -> True
                    ((_, firstOuterShared, _):_, (_, secondOuterShared, _):_) ->
                        all (`notElem` secondOuterShared) firstOuterShared
                    _ -> False
            _ -> False)
    , scalarSource "twice/int/full"
        (sourceTwiceSum . Source.twice (undefined :: Int) . countValues)
        (selectedTwice >=> forceTwiceSum)
    , scalarSource "twice/integer/full"
        (fromInteger . sourceTwiceSum . Source.twice (undefined :: Integer) . countValues) $ \sample -> do
        input <- M.lazy (pure (undefined :: Integer))
        counts <- M.lazy (countList (countValues sample))
        fromInteger <$> (genericTwice input counts >>= forceTwiceSum)
    , snapshot "twice/distinct-map-and-helper-results-shared-fields" $ \sample -> M.value $ M.eval1 $ do
        M.PairT firstMap secondMap <- selectedTwice sample
        firstNames <- M.force firstMap >>= manyFieldNames
        secondNames <- M.force secondMap >>= manyFieldNames
        pure (M.tname firstMap /= M.tname secondMap && case (firstNames, secondNames) of
            (Just firstEntries, Just secondEntries) ->
                sameSharedFields (firstEntries ++ secondEntries) &&
                distinctHelperResults (firstEntries ++ secondEntries)
            _ -> False)
    , scalarSource "cached-map/int/full"
        (sourceTwiceSum . Source.cachedMap (undefined :: Int) . countValues)
        (selectedCachedMap >=> forceTwiceSum)
    , snapshot "cached-map/shared-map-and-helper-results" $ \sample -> M.value $ M.eval1 $ do
        M.PairT firstMap secondMap <- selectedCachedMap sample
        firstNames <- M.force firstMap >>= manyFieldNames
        secondNames <- M.force secondMap >>= manyFieldNames
        pure (M.tname firstMap == M.tname secondMap && case (firstNames, secondNames) of
            (Just firstEntries, Just secondEntries) ->
                firstEntries == secondEntries && sameSharedFields firstEntries &&
                distinctHelperResults firstEntries
            _ -> False)
    , scalarSource "separate-many/int/full"
        (sourceSum . Source.separateMany (undefined :: Int) . countValues)
        (selectedSeparateMany >=> forceSum)
    , snapshot "separate-many/distinct-bindings-shared-across-helpers" $ \sample -> M.value $ M.eval1 $ do
        names <- selectedSeparateMany sample >>= manyFieldNames
        pure (case names of
            Nothing -> False
            Just [] -> True
            Just entries@((_, leftShared, rightShared):_) ->
                all (\(_, left, right) -> left == leftShared && right == rightShared) entries &&
                all (`notElem` rightShared) leftShared && distinctHelperResults entries)
    , scalarSource "mixed/int/full"
        (sourceMixedSum . Source.mixed (undefined :: Int) . countValues)
        (selectedMixed >=> forceMixedSum)
    , snapshot "mixed/distinct-helpers-and-shared-fields" $ \sample -> M.value $ M.eval1 $ do
        M.PairT directHelper mappedHelpers <- selectedMixed sample
        directNames <- M.force directHelper >>= fieldNames
        mappedNames <- M.force mappedHelpers >>= manyFieldNames
        pure (case (directNames, mappedNames) of
            (Just (directLeft, directRight), Just mappedEntries) ->
                directLeft == directRight &&
                all (\(_, left, right) -> left == directLeft && right == directLeft) mappedEntries &&
                let roots = M.tname directHelper : helperResultNames mappedEntries
                in length roots == length (nub roots)
            _ -> False)
    , demanded "broken-head/all-helper-maybe-and-list-spines"
        projectPayloadSpines demandPayloadSpines selectedBrokenHead
    , failure "broken-head/full" (selectedBrokenHead >=> forceSum)
    , demanded "broken-root/all-helper-roots"
        (projectManyRoots True) (demandManyRoots True) selectedBrokenRoot
    , failure "broken-root/full" (selectedBrokenRoot >=> forceSum)
    ]

sourceRepeatedMany :: Sample -> Int
sourceRepeatedMany sample = max 1 (repeats sample) * sourceSum (Source.many (undefined :: Int) (countValues sample))

sourceTwiceSum :: Num a => ([(Maybe [a], Maybe [a])], [(Maybe [a], Maybe [a])]) -> a
sourceTwiceSum (first, second) = sourceSum first + sourceSum second

sourceMixedSum :: Num a => ((Maybe [a], Maybe [a]), [(Maybe [a], Maybe [a])]) -> a
sourceMixedSum (first, rest) = sourceSum [first] + sourceSum rest

forceTwiceSum :: Num a => M.PairT (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))) (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))) -> M.MemoristM a
forceTwiceSum (M.PairT first second) = do
    left <- M.force first >>= forceSum
    right <- M.force second >>= forceSum
    pure (left + right)

forceMixedSum :: Num a => M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))) -> M.MemoristM a
forceMixedSum (M.PairT first rest) = do
    end <- M.lazy (pure L.NilT)
    firstValue <- forceSum (L.ConsT first end)
    restValue <- M.force rest >>= forceSum
    pure (firstValue + restValue)

genericTwice :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th (L.ListT Int) -> M.MemoristM (M.PairT (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))) (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))))
genericTwice = twiceM

genericCachedMap :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th (L.ListT Int) -> M.MemoristM (M.PairT (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))) (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))))
genericCachedMap = cachedMapM

genericSeparateMany :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th (L.ListT Int) -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
genericSeparateMany = separateManyM

genericMixed :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th (L.ListT Int) -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))))
genericMixed = mixedM

genericBrokenHead :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th (L.ListT Int) -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
genericBrokenHead = brokenHeadM

genericBrokenRoot :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th (L.ListT Int) -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
genericBrokenRoot = brokenRootM

withCounts :: (M.Th Int -> M.Th (L.ListT Int) -> M.MemoristM result) -> Sample -> M.MemoristM result
withCounts caller sample = do
    input <- M.lazy (pure (undefined :: Int))
    counts <- M.lazy (countList (countValues sample))
    caller input counts

selectedTwice :: Sample -> M.MemoristM (M.PairT (L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)))) (L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)))))
selectedTwice = withCounts genericTwice

selectedCachedMap :: Sample -> M.MemoristM (M.PairT (L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)))) (L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)))))
selectedCachedMap = withCounts genericCachedMap

selectedSeparateMany :: Sample -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))))
selectedSeparateMany = withCounts genericSeparateMany

selectedMixed :: Sample -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) (L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)))))
selectedMixed = withCounts genericMixed

selectedBrokenHead :: Sample -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))))
selectedBrokenHead sample = do
    input <- M.lazy (pure (undefined :: Int))
    counts <- M.lazy (countList (countValues (sample {width = max 1 (width sample)})))
    genericBrokenHead input counts

selectedBrokenRoot :: Sample -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))))
selectedBrokenRoot sample = do
    input <- M.lazy (pure (undefined :: Int))
    counts <- M.lazy (countList (countValues (sample {width = max 1 (width sample)})))
    genericBrokenRoot input counts

sameSharedFields :: [(M.ThName, [M.ThName], [M.ThName])] -> Bool
sameSharedFields [] = True
sameSharedFields entries@((_, shared, _):_) =
    all (\(_, left, right) -> left == shared && right == shared) entries

helperResultNames :: [(M.ThName, [M.ThName], [M.ThName])] -> [M.ThName]
helperResultNames = map (\(root, _, _) -> root)

distinctHelperResults :: [(M.ThName, [M.ThName], [M.ThName])] -> Bool
distinctHelperResults entries =
    let roots = helperResultNames entries
    in length roots == length (nub roots)

projectPayloadSpines :: Sample -> L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) -> [(Maybe Int, Maybe Int)]
projectPayloadSpines _ = walk
  where
    walk L.NilT = []
    walk (L.ConsT headThunk tailThunk) =
        let M.PairT first second = M.tval headThunk
        in (payload first, payload second) : walk (M.tval tailThunk)
    payload root = case M.tval root of
        M.NothingT -> Nothing
        M.JustT list -> Just (spineLength (M.tval list))
    spineLength L.NilT = 0
    spineLength (L.ConsT _ tailThunk) = 1 + spineLength (M.tval tailThunk)

demandPayloadSpines :: Sample -> L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) -> L.ListD (M.PairD (M.MaybeD (L.ListD Int)) (M.MaybeD (L.ListD Int)))
demandPayloadSpines _ = walk
  where
    walk L.NilT = L.NilD
    walk (L.ConsT headThunk tailThunk) = L.ConsD
        (M.Need (case M.tval headThunk of
            M.PairT first second -> M.PairD (payload first) (payload second)))
        (M.Need (walk (M.tval tailThunk)))
    payload root = M.Need (case M.tval root of
        M.NothingT -> M.NothingD
        M.JustT list -> M.JustD (M.Need (spine (M.tval list))))
    spine L.NilT = L.NilD
    spine (L.ConsT _ tailThunk) = L.ConsD M.Skip (M.Need (spine (M.tval tailThunk)))
