module Case (probes) where

import Control.Monad ((>=>), replicateM)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import qualified Example as Source
import Generated (Scalar, oneM, manyM, twiceZipM, cachedZipM, separateManyM, brokenHeadM, brokenRootM, wrappedM)
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, failure, scalarSource, snapshot)

probes :: [Probe]
probes =
    [ scalarSource "one/int/full-unused-input-and-count"
        (const (sourceOneSum (Source.one (undefined :: Int) undefined)))
        (selectedOne >=> forceOneSum)
    , scalarSource "one/integer/full-unused-input-and-count"
        (const (fromInteger (sourceOneSum (Source.one (undefined :: Integer) undefined)))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Integer))
        n <- M.lazy (pure (undefined :: Int))
        fromInteger <$> (genericOne input n >>= forceOneSum)
    , scalarSource "many/int/full-unused-input-and-count-elements"
        (\sample -> sourceManySum (Source.many (bottomValues sample :: [Int]) (bottomValues sample)))
        (selectedMany >=> forceManySum)
    , scalarSource "many/integer/full-unused-input-and-count-elements"
        (\sample -> fromInteger (sourceManySum (Source.many (bottomValues sample :: [Integer]) (bottomValues sample)))) $ \sample -> do
        inputs <- M.lazy (thunkList (bottomValues sample :: [Integer]))
        counts <- M.lazy (thunkList (bottomValues sample :: [Int]))
        fromInteger <$> (genericMany inputs counts >>= forceManySum)
    , demanded "many/complete-outer-spine" (projectManyRoots False) (demandManyRoots False) selectedMany
    , demanded "many/all-helper-result-roots" (projectManyRoots True) (demandManyRoots True) selectedMany
    , snapshot "one/distinct-helper-results/shared-maybe-list-head-and-tail-identities" $ \sample -> M.value $ M.eval1 $ do
        M.PairT leftResult rightResult <- selectedOne sample
        leftNames <- M.force leftResult >>= fieldNames
        rightNames <- M.force rightResult >>= fieldNames
        pure (M.tname leftResult /= M.tname rightResult && case (leftNames, rightNames) of
            (Just (leftFirst, leftSecond), Just (rightFirst, rightSecond)) ->
                leftFirst == leftSecond && leftFirst == rightFirst && leftFirst == rightSecond
            _ -> False)
    , snapshot "many/distinct-helper-results/shared-maybe-list-head-and-tail-identities" $ \sample -> M.value $ M.eval1 $ do
        names <- selectedMany sample >>= manyFieldNames
        pure (case names of
            Nothing -> False
            Just [] -> True
            Just entries@((_, shared, _):_) ->
                all (\(_, left, right) -> left == shared && right == shared) entries &&
                let roots = map (\(root, _, _) -> root) entries
                in length roots == length (nub roots))
    ]
    ++ preservationProbes

genericOne :: (Eq a, Num a, Scalar a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
genericOne = oneM

genericMany :: (Eq a, Num a, Scalar a) => M.Th (L.ListT a) -> M.Th (L.ListT Int) -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
genericMany = manyM

sourcePairSum :: Num a => (Maybe [a], Maybe [a]) -> a
sourcePairSum (first, second) = sum (fromMaybe [] first) + sum (fromMaybe [] second)

sourceOneSum :: Num a => ((Maybe [a], Maybe [a]), (Maybe [a], Maybe [a])) -> a
sourceOneSum (first, second) = sourcePairSum first + sourcePairSum second

sourceManySum :: Num a => [(Maybe [a], Maybe [a])] -> a
sourceManySum = sum . map sourcePairSum

forcePairSum :: Num a => M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)) -> M.MemoristM a
forcePairSum (M.PairT first second) = do
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

forceOneSum :: Num a => M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) -> M.MemoristM a
forceOneSum (M.PairT first second) = do
    left <- M.force first >>= forcePairSum
    right <- M.force second >>= forcePairSum
    pure (left + right)

forceManySum :: Num a => L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) -> M.MemoristM a
forceManySum L.NilT = pure 0
forceManySum (L.ConsT headThunk tailThunk) = do
    value <- M.force headThunk >>= forcePairSum
    rest <- M.force tailThunk >>= forceManySum
    pure (value + rest)

bottomValues :: Sample -> [a]
bottomValues sample = replicate (max 0 (width sample)) undefined

thunkList :: [a] -> M.MemoristM (L.ListT a)
thunkList [] = pure L.NilT
thunkList (first:rest) = do
    headThunk <- M.lazy (pure first)
    tailThunk <- M.lazy (thunkList rest)
    pure (L.ConsT headThunk tailThunk)

selectedOne :: Sample -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))))
selectedOne _ = do
    input <- M.lazy (pure (undefined :: Int))
    n <- M.lazy (pure (undefined :: Int))
    genericOne input n

selectedMany :: Sample -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))))
selectedMany sample = do
    inputs <- M.lazy (thunkList (bottomValues sample :: [Int]))
    counts <- M.lazy (thunkList (bottomValues sample :: [Int]))
    genericMany inputs counts

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
    [ demanded "one/selected-helper-result-roots" projectOneHelpers demandOneHelpers selectedOne
    , scalarSource "many/repeated-fresh-outer-calls" sourceRepeatedMany $ \sample ->
        withBottomLists (\inputs counts ->
            sum <$> replicateM (max 1 (repeats sample)) (genericMany inputs counts >>= forceManySum)) sample
    , scalarSource "many/repeated-cached-outer-call" sourceRepeatedMany $ \sample ->
        withBottomLists (\inputs counts -> do
            cached <- M.lazy (genericMany inputs counts)
            sum <$> replicateM (max 1 (repeats sample)) (M.force cached >>= forceManySum)) sample
    , snapshot "many/fresh-outer-identities" $ \sample -> M.value $ M.eval1 $
        withBottomLists (\inputs counts -> do
            firstNames <- genericMany inputs counts >>= manyFieldNames
            secondNames <- genericMany inputs counts >>= manyFieldNames
            pure (case (firstNames, secondNames) of
                (Just firstEntries, Just secondEntries) ->
                    mappedHelpersSharePayload firstEntries && mappedHelpersSharePayload secondEntries &&
                    distinctThunkNames (mappedHelperRoots (firstEntries ++ secondEntries)) &&
                    outerPayloadVectorsAreDisjoint firstEntries secondEntries
                _ -> False)) sample
    , scalarSource "one/repeated-fresh-outer-calls" sourceRepeatedOne $ \sample ->
        withBottomScalars $ \input n ->
            sum <$> replicateM (max 1 (repeats sample)) (genericOne input n >>= forceOneSum)
    , scalarSource "one/repeated-cached-outer-call" sourceRepeatedOne $ \sample ->
        withBottomScalars $ \input n -> do
            cached <- M.lazy (genericOne input n)
            sum <$> replicateM (max 1 (repeats sample)) (M.force cached >>= forceOneSum)
    , snapshot "one/fresh-outer-identities" $ \_ -> M.value $ M.eval1 $
        withBottomScalars $ \input n -> do
            firstOuter@(M.PairT firstZipResult firstDirectResult) <- genericOne input n
            secondOuter@(M.PairT secondZipResult secondDirectResult) <- genericOne input n
            firstNames <- oneFieldNames firstOuter
            secondNames <- oneFieldNames secondOuter
            pure (distinctThunkNames (map M.tname
                [firstZipResult, firstDirectResult, secondZipResult, secondDirectResult]) &&
                case (firstNames, secondNames) of
                    (Just firstVectors@((firstShared, _), _), Just secondVectors@((secondShared, _), _)) ->
                        onePayloadVectorsAreShared firstVectors && onePayloadVectorsAreShared secondVectors &&
                        all (`notElem` secondShared) firstShared
                    _ -> False)
    , scalarSource "twice-zip/int/full"
        (\sample -> sourceZipPairSum (Source.twiceZip (bottomValues sample :: [Int]) (bottomValues sample)))
        (selectedTwiceZip >=> forceZipPairSum)
    , snapshot "twice-zip/distinct-map-and-helper-results/shared-fields" $ \sample -> M.value $ M.eval1 $ do
        M.PairT firstMap secondMap <- selectedTwiceZip sample
        firstNames <- M.force firstMap >>= manyFieldNames
        secondNames <- M.force secondMap >>= manyFieldNames
        pure (M.tname firstMap /= M.tname secondMap && case (firstNames, secondNames) of
            (Just firstEntries, Just secondEntries) ->
                mappedHelpersSharePayload (firstEntries ++ secondEntries) &&
                distinctThunkNames (mappedHelperRoots (firstEntries ++ secondEntries))
            _ -> False)
    , scalarSource "cached-zip/int/full"
        (\sample -> sourceZipPairSum (Source.cachedZip (bottomValues sample :: [Int]) (bottomValues sample)))
        (selectedCachedZip >=> forceZipPairSum)
    , snapshot "cached-zip/shared-map-and-helper-results/shared-fields" $ \sample -> M.value $ M.eval1 $ do
        M.PairT firstMap secondMap <- selectedCachedZip sample
        firstNames <- M.force firstMap >>= manyFieldNames
        secondNames <- M.force secondMap >>= manyFieldNames
        pure (M.tname firstMap == M.tname secondMap && case (firstNames, secondNames) of
            (Just firstEntries, Just secondEntries) ->
                firstEntries == secondEntries && mappedHelpersSharePayload firstEntries &&
                distinctThunkNames (mappedHelperRoots firstEntries)
            _ -> False)
    , scalarSource "separate-many/int/full"
        (\sample -> sourceManySum (Source.separateMany (bottomValues sample :: [Int]) (bottomValues sample)))
        (selectedSeparateMany >=> forceManySum)
    , snapshot "separate-many/distinct-bindings/shared-across-helpers" $ \sample -> M.value $ M.eval1 $ do
        names <- selectedSeparateMany sample >>= manyFieldNames
        pure (case names of
            Just entries -> separatePayloadBindingsAreShared entries && distinctThunkNames (mappedHelperRoots entries)
            Nothing -> False)
    , demanded "broken-head/all-helper-maybe-and-complete-list-spines"
        projectProtectedPayloadSpines demandProtectedPayloadSpines selectedBrokenHead
    , failure "broken-head/full" (selectedBrokenHead >=> forceManySum)
    , demanded "broken-root/all-helper-result-roots"
        (projectManyRoots True) (demandManyRoots True) selectedBrokenRoot
    , failure "broken-root/full" (selectedBrokenRoot >=> forceManySum)
    , scalarSource "many/empty-left/unused-right-list"
        (const (sourceManySum (Source.many ([] :: [Int]) undefined))) $ \_ -> do
        inputs <- M.lazy (thunkList ([] :: [Int]))
        counts <- M.lazy (pure (undefined :: L.ListT Int))
        genericMany inputs counts >>= forceManySum
    , scalarSource "many/empty-right/unused-left-elements-and-tail"
        (const (sourceManySum (Source.many (undefined : undefined :: [Int]) []))) $ \_ -> do
        inputs <- M.lazy $ do
            headThunk <- M.lazy (pure (undefined :: Int))
            tailThunk <- M.lazy (pure (undefined :: L.ListT Int))
            pure (L.ConsT headThunk tailThunk)
        counts <- M.lazy (thunkList ([] :: [Int]))
        genericMany inputs counts >>= forceManySum
    , scalarSource "wrapped/int/full-unused-input-and-count-elements"
        (\sample -> sourceManySum (Source.wrapped (bottomValues sample :: [Int]) (bottomValues sample)))
        (selectedWrapped >=> forceManySum)
    ]

projectOneHelpers :: Sample -> M.PairT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) -> (Maybe Bool, Maybe Bool)
projectOneHelpers sample (M.PairT leftResult rightResult) =
    (project (leftDemand sample) leftResult, project (rightDemand sample) rightResult)
  where
    project False _ = Nothing
    project True result = Just (case M.tval result of M.PairT _ _ -> True)

demandOneHelpers :: Sample -> M.PairT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) -> M.PairD (M.PairD (M.MaybeD (L.ListD Int)) (M.MaybeD (L.ListD Int))) (M.PairD (M.MaybeD (L.ListD Int)) (M.MaybeD (L.ListD Int)))
demandOneHelpers sample _ = M.PairD (demand (leftDemand sample)) (demand (rightDemand sample))
  where
    demand False = M.Skip
    demand True = M.Need (M.PairD M.Skip M.Skip)

sourceRepeatedMany :: Sample -> Int
sourceRepeatedMany sample = max 1 (repeats sample) *
    sourceManySum (Source.many (bottomValues sample :: [Int]) (bottomValues sample))

sourceRepeatedOne :: Sample -> Int
sourceRepeatedOne sample = max 1 (repeats sample) * sourceOneSum (Source.one (undefined :: Int) undefined)

withBottomLists :: (M.Th (L.ListT Int) -> M.Th (L.ListT Int) -> M.MemoristM result) -> Sample -> M.MemoristM result
withBottomLists program sample = do
    inputs <- M.lazy (thunkList (bottomValues sample :: [Int]))
    counts <- M.lazy (thunkList (bottomValues sample :: [Int]))
    program inputs counts

withBottomScalars :: (M.Th Int -> M.Th Int -> M.MemoristM result) -> M.MemoristM result
withBottomScalars program = do
    input <- M.lazy (pure (undefined :: Int))
    n <- M.lazy (pure (undefined :: Int))
    program input n

mappedHelpersSharePayload :: [(M.ThName, [M.ThName], [M.ThName])] -> Bool
mappedHelpersSharePayload [] = True
mappedHelpersSharePayload entries@((_, shared, _):_) =
    all (\(_, left, right) -> left == shared && right == shared) entries

mappedHelperRoots :: [(M.ThName, [M.ThName], [M.ThName])] -> [M.ThName]
mappedHelperRoots = map (\(root, _, _) -> root)

distinctThunkNames :: [M.ThName] -> Bool
distinctThunkNames names = length names == length (nub names)

outerPayloadVectorsAreDisjoint :: [(M.ThName, [M.ThName], [M.ThName])] -> [(M.ThName, [M.ThName], [M.ThName])] -> Bool
outerPayloadVectorsAreDisjoint [] [] = True
outerPayloadVectorsAreDisjoint ((_, firstShared, _):_) ((_, secondShared, _):_) =
    all (`notElem` secondShared) firstShared
outerPayloadVectorsAreDisjoint _ _ = False

oneFieldNames :: M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) -> M.MemoristM (Maybe (([M.ThName], [M.ThName]), ([M.ThName], [M.ThName])))
oneFieldNames (M.PairT zipResult directResult) = do
    zipNames <- M.force zipResult >>= fieldNames
    directNames <- M.force directResult >>= fieldNames
    pure ((,) <$> zipNames <*> directNames)

onePayloadVectorsAreShared :: (([M.ThName], [M.ThName]), ([M.ThName], [M.ThName])) -> Bool
onePayloadVectorsAreShared ((zipLeft, zipRight), (directLeft, directRight)) =
    zipLeft == zipRight && zipLeft == directLeft && zipLeft == directRight

sourceZipPairSum :: Num a => ([(Maybe [a], Maybe [a])], [(Maybe [a], Maybe [a])]) -> a
sourceZipPairSum (first, second) = sourceManySum first + sourceManySum second

forceZipPairSum :: Num a => M.PairT (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))) (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))) -> M.MemoristM a
forceZipPairSum (M.PairT first second) = do
    left <- M.force first >>= forceManySum
    right <- M.force second >>= forceManySum
    pure (left + right)

genericTwiceZip :: (Eq a, Num a, Scalar a) => M.Th (L.ListT a) -> M.Th (L.ListT Int) -> M.MemoristM (M.PairT (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))) (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))))
genericTwiceZip = twiceZipM

genericCachedZip :: (Eq a, Num a, Scalar a) => M.Th (L.ListT a) -> M.Th (L.ListT Int) -> M.MemoristM (M.PairT (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))) (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a)))))
genericCachedZip = cachedZipM

genericSeparateMany :: (Eq a, Num a, Scalar a) => M.Th (L.ListT a) -> M.Th (L.ListT Int) -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
genericSeparateMany = separateManyM

genericBrokenHead :: (Eq a, Num a, Scalar a) => M.Th (L.ListT a) -> M.Th (L.ListT Int) -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
genericBrokenHead = brokenHeadM

genericBrokenRoot :: (Eq a, Num a, Scalar a) => M.Th (L.ListT a) -> M.Th (L.ListT Int) -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
genericBrokenRoot = brokenRootM

genericWrapped :: (Eq a, Num a, Scalar a) => M.Th (L.ListT a) -> M.Th (L.ListT Int) -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
genericWrapped = wrappedM

selectedTwiceZip :: Sample -> M.MemoristM (M.PairT (L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)))) (L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)))))
selectedTwiceZip = withBottomLists genericTwiceZip

selectedCachedZip :: Sample -> M.MemoristM (M.PairT (L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)))) (L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)))))
selectedCachedZip = withBottomLists genericCachedZip

selectedSeparateMany :: Sample -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))))
selectedSeparateMany = withBottomLists genericSeparateMany

selectedBrokenHead :: Sample -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))))
selectedBrokenHead sample = withBottomLists genericBrokenHead (sample {width = max 1 (width sample)})

selectedBrokenRoot :: Sample -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))))
selectedBrokenRoot sample = withBottomLists genericBrokenRoot (sample {width = max 1 (width sample)})

selectedWrapped :: Sample -> M.MemoristM (L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))))
selectedWrapped = withBottomLists genericWrapped

separatePayloadBindingsAreShared :: [(M.ThName, [M.ThName], [M.ThName])] -> Bool
separatePayloadBindingsAreShared [] = True
separatePayloadBindingsAreShared entries@((_, leftBinding, rightBinding):_) =
    all (\(_, left, right) -> left == leftBinding && right == rightBinding) entries &&
    all (`notElem` rightBinding) leftBinding

projectProtectedPayloadSpines :: Sample -> L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) -> [(Maybe Int, Maybe Int)]
projectProtectedPayloadSpines _ = walk
  where
    walk L.NilT = []
    walk (L.ConsT headThunk tailThunk) =
        let M.PairT left right = M.tval headThunk
        in (payload left, payload right) : walk (M.tval tailThunk)
    payload root = case M.tval root of
        M.NothingT -> Nothing
        M.JustT list -> Just (spineLength (M.tval list))
    spineLength L.NilT = 0
    spineLength (L.ConsT _ tailThunk) = 1 + spineLength (M.tval tailThunk)

demandProtectedPayloadSpines :: Sample -> L.ListT (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int))) -> L.ListD (M.PairD (M.MaybeD (L.ListD Int)) (M.MaybeD (L.ListD Int)))
demandProtectedPayloadSpines _ = walk
  where
    walk L.NilT = L.NilD
    walk (L.ConsT headThunk tailThunk) = L.ConsD
        (M.Need (case M.tval headThunk of
            M.PairT left right -> M.PairD (payload left) (payload right)))
        (M.Need (walk (M.tval tailThunk)))
    payload root = M.Need (case M.tval root of
        M.NothingT -> M.NothingD
        M.JustT list -> M.JustD (M.Need (spine (M.tval list))))
    spine L.NilT = L.NilD
    spine (L.ConsT _ tailThunk) = L.ConsD M.Skip (M.Need (spine (M.tval tailThunk)))
