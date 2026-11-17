module Case (probes) where

import Control.Monad ((>=>))
import Data.List (nub)
import Data.Maybe (fromMaybe)
import qualified Example as Source
import Generated (Scalar, oneM, manyM)
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, scalarSource, snapshot)

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
