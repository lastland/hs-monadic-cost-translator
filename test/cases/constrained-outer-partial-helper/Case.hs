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
