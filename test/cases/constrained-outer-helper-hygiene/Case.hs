module Case (probes) where

import qualified Example as Source
import Generated (Scalar, PairT(..), oneM)
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalar, scalarSource, snapshot)

probes :: [Probe]
probes =
    [ scalarSource "one/int/full"
        (sourceSum id . Source.one . other) $ \sample -> do
        input <- M.lazy (pure (other sample))
        generic input >>= forceSum M.force
    , scalarSource "one/integer/full"
        (fromInteger . sourceSum id . Source.one . toInteger . other) $ \sample -> do
        input <- M.lazy (pure (toInteger (other sample)))
        fromInteger <$> (generic input >>= forceSum M.force)
    , scalarSource "one/abstract-only/bottom-captured"
        (const (sourceSum (const 0) (Source.one (undefined :: Int)))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        generic input >>= forceSum (const (pure 0))
    , scalar "one/constructor-only/bottom-captured" $ \_ -> do
        input <- M.lazy (pure (undefined :: Int))
        generic input >>= forceConstructors
    , snapshot "one/shared-fields-and-captured-input" $ \_ -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int))
        names <- generic input >>= fieldNames
        pure (case names of
            Just (first@[_, _, left, right], second) ->
                first == second && left /= right && right == M.tname input
            _ -> False)
    ]

generic :: (Eq a, Num a, Scalar a) => M.Th a -> M.MemoristM (M.PairT (M.MaybeT (PairT a a)) (M.MaybeT (PairT a a)))
generic = oneM

sourceSum :: Num a => (a -> a) -> (Maybe (Source.Pair a a), Maybe (Source.Pair a a)) -> a
sourceSum projectCaptured (first, second) = payloadSum first + payloadSum second
  where
    payloadSum Nothing = 0
    payloadSum (Just (Source.Pair left right)) = left + projectCaptured right

forceSum :: Num a => (M.Th a -> M.MemoristM a) -> M.PairT (M.MaybeT (PairT a a)) (M.MaybeT (PairT a a)) -> M.MemoristM a
forceSum projectCaptured (M.PairT first second) = do
    left <- M.force first >>= payloadSum
    right <- M.force second >>= payloadSum
    pure (left + right)
  where
    payloadSum M.NothingT = pure 0
    payloadSum (M.JustT pair) = do
        PairT left right <- M.force pair
        abstract <- M.force left
        captured <- projectCaptured right
        pure (abstract + captured)

forceConstructors :: M.PairT (M.MaybeT (PairT a a)) (M.MaybeT (PairT a a)) -> M.MemoristM Int
forceConstructors (M.PairT first second) = do
    left <- M.force first >>= payloadRoot
    right <- M.force second >>= payloadRoot
    pure (left + right)
  where
    payloadRoot M.NothingT = pure 0
    payloadRoot (M.JustT pair) = do
        PairT _ _ <- M.force pair
        pure 1

fieldNames :: M.PairT (M.MaybeT (PairT a a)) (M.MaybeT (PairT a a)) -> M.MemoristM (Maybe ([M.ThName], [M.ThName]))
fieldNames (M.PairT first second) = do
    left <- M.force first
    right <- M.force second
    case (left, right) of
        (M.JustT leftPair, M.JustT rightPair) -> do
            PairT leftAbstract leftCaptured <- M.force leftPair
            PairT rightAbstract rightCaptured <- M.force rightPair
            pure (Just
                ( [M.tname first, M.tname leftPair, M.tname leftAbstract, M.tname leftCaptured]
                , [M.tname second, M.tname rightPair, M.tname rightAbstract, M.tname rightCaptured]
                ))
        _ -> pure Nothing
