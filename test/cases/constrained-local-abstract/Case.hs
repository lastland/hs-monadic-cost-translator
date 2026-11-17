module Case (probes) where

import qualified Example as Source
import Generated (Scalar, oneM)
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, scalarSource, snapshot)

probes :: [Probe]
probes =
    [ scalarSource "one/int"
        (const (sourceSum (Source.one (undefined :: Int)))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        generic input >>= forceSum
    , scalarSource "one/integer"
        (const (fromInteger (sourceSum (Source.one (undefined :: Integer))))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Integer))
        fromInteger <$> (generic input >>= forceSum)
    , demanded "one/selected-fields" projectSelected demandSelected $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        generic input
    , snapshot "one/field-aliases" $ \_ -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        M.PairT first second <- generic input
        pure (M.tname first == M.tname second)
    , snapshot "one/fresh-invocations" $ \_ -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        M.PairT firstLeft firstRight <- generic input
        M.PairT secondLeft secondRight <- generic input
        pure (all (`notElem` [M.tname secondLeft, M.tname secondRight])
            [M.tname firstLeft, M.tname firstRight])
    ]

generic :: (Eq a, Num a, Scalar a) => M.Th a -> M.MemoristM (M.PairT a a)
generic = oneM

sourceSum :: Num a => (a, a) -> a
sourceSum (first, second) = first + second

forceSum :: Num a => M.PairT a a -> M.MemoristM a
forceSum (M.PairT first second) = do
    a <- M.force first
    b <- M.force second
    pure (a + b)

projectSelected :: Sample -> M.PairT Int Int -> (Maybe Int, Maybe Int)
projectSelected sample (M.PairT first second) =
    (if leftDemand sample then Just (M.tval first) else Nothing,
     if rightDemand sample then Just (M.tval second) else Nothing)

demandSelected :: Sample -> M.PairT Int Int -> M.PairD Int Int
demandSelected sample (M.PairT first second) = M.PairD
    (if leftDemand sample then M.fullDemand first else M.Skip)
    (if rightDemand sample then M.fullDemand second else M.Skip)
