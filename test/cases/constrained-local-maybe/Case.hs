module Case (probes) where

import Control.Monad (replicateM)
import Data.Maybe (fromMaybe)
import qualified Example as Source
import Generated (viewsM, brokenM)
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, failure, scalarSource, snapshot)

probes :: [Probe]
probes =
    [ scalarSource "just/full"
        (sourceSum (undefined :: Bool) . positiveCount) $ \sample -> do
        input <- M.lazy (pure (undefined :: Bool))
        n <- M.lazy (pure (positiveCount sample))
        generic input n >>= forceSum
    , scalarSource "nothing/full"
        (const (sourceSum (undefined :: Int -> Int) 0)) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int -> Int))
        n <- M.lazy (pure 0)
        generic input n >>= forceSum
    , demanded "selected/payloads" projectPayloads demandPayloads selectedViews
    , demanded "selected/roots" projectRoots demandRoots selectedViews
    , snapshot "just/root-and-payload-aliases" $ \sample -> M.value $ M.eval1 $ do
        M.PairT first second <- selectedJust sample
        left <- M.force first
        right <- M.force second
        pure (M.tname first == M.tname second && case (left, right) of
            (M.JustT a, M.JustT b) -> M.tname a == M.tname b
            _ -> False)
    , scalarSource "just/repeated-fresh-calls"
        (\sample -> repeats sample * sourceSum (undefined :: Bool) (positiveCount sample)) $ \sample -> do
        input <- M.lazy (pure (undefined :: Bool))
        n <- M.lazy (pure (positiveCount sample))
        sum <$> replicateM (repeats sample) (generic input n >>= forceSum)
    , scalarSource "just/repeated-cached-call"
        (\sample -> repeats sample * sourceSum (undefined :: Bool) (positiveCount sample)) $ \sample -> do
        input <- M.lazy (pure (undefined :: Bool))
        n <- M.lazy (pure (positiveCount sample))
        cached <- M.lazy (generic input n)
        sum <$> replicateM (repeats sample) (M.force cached >>= forceSum)
    , snapshot "just/fresh-root-and-payload-identities" $ \sample -> M.value $ M.eval1 $ do
        input <- M.lazy (pure (undefined :: Int -> Int))
        n <- M.lazy (pure (positiveCount sample))
        M.PairT first _ <- generic input n
        M.PairT second _ <- generic input n
        left <- M.force first
        right <- M.force second
        pure (case (left, right) of
            (M.JustT a, M.JustT b) ->
                all (`notElem` [M.tname second, M.tname b]) [M.tname first, M.tname a]
            _ -> False)
    , demanded "undefined-count/skipped" (\_ _ -> ())
        (\_ _ -> M.PairD M.Skip M.Skip) $ \_ -> do
        input <- M.lazy (pure (undefined :: Bool))
        n <- M.lazy (pure (undefined :: Int))
        generic input n
    , failure "undefined-count/demanded" $ \_ -> do
        input <- M.lazy (pure (undefined :: Bool))
        n <- M.lazy (pure (undefined :: Int))
        generic input n >>= forceSum
    , demanded "broken/both-roots" (\_ _ -> ())
        (\_ _ -> M.PairD (M.Need (M.JustD M.Skip)) (M.Need (M.JustD M.Skip))) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int -> Int))
        genericBroken input
    , failure "broken/payload" $ \_ -> do
        input <- M.lazy (pure (undefined :: Int -> Int))
        genericBroken input >>= forceSum
    , demanded "broken/skipped" (\_ _ -> ())
        (\_ _ -> M.PairD M.Skip M.Skip) $ \_ -> do
        input <- M.lazy (pure (undefined :: Int -> Int))
        genericBroken input
    ]

generic :: M.Th p -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT Int) (M.MaybeT Int))
generic = viewsM

genericBroken :: M.Th p -> M.MemoristM (M.PairT (M.MaybeT Int) (M.MaybeT Int))
genericBroken = brokenM

positiveCount :: Sample -> Int
positiveCount sample = max 1 (number sample)

sourceSum :: p -> Int -> Int
sourceSum input n = case Source.views input n of
    (first, second) -> fromMaybe 0 first + fromMaybe 0 second

forceSum :: M.PairT (M.MaybeT Int) (M.MaybeT Int) -> M.MemoristM Int
forceSum (M.PairT first second) = do
    left <- M.force first >>= payloadValue
    right <- M.force second >>= payloadValue
    pure (left + right)
  where
    payloadValue M.NothingT = pure 0
    payloadValue (M.JustT payload) = M.force payload

selectedViews :: Sample -> M.MemoristM (M.PairT (M.MaybeT Int) (M.MaybeT Int))
selectedViews sample = do
    input <- M.lazy (pure (undefined :: Bool))
    n <- M.lazy (pure (if even (variant sample) then 0 else positiveCount sample))
    generic input n

selectedJust :: Sample -> M.MemoristM (M.PairT (M.MaybeT Int) (M.MaybeT Int))
selectedJust sample = do
    input <- M.lazy (pure (undefined :: Int -> Int))
    n <- M.lazy (pure (positiveCount sample))
    generic input n

projectPayloads :: Sample -> M.PairT (M.MaybeT Int) (M.MaybeT Int) -> (Maybe (Maybe Int), Maybe (Maybe Int))
projectPayloads sample (M.PairT first second) =
    (project (leftDemand sample) first, project (rightDemand sample) second)
  where
    project False _ = Nothing
    project True root = Just (case M.tval root of
        M.NothingT -> Nothing
        M.JustT payload -> Just (M.tval payload))

demandPayloads :: Sample -> M.PairT (M.MaybeT Int) (M.MaybeT Int) -> M.PairD (M.MaybeD Int) (M.MaybeD Int)
demandPayloads sample (M.PairT first second) = M.PairD
    (if leftDemand sample then M.fullDemand first else M.Skip)
    (if rightDemand sample then M.fullDemand second else M.Skip)

projectRoots :: Sample -> M.PairT (M.MaybeT Int) (M.MaybeT Int) -> (Maybe Bool, Maybe Bool)
projectRoots sample (M.PairT first second) =
    (project (leftDemand sample) first, project (rightDemand sample) second)
  where
    project False _ = Nothing
    project True root = Just (case M.tval root of
        M.NothingT -> False
        M.JustT _ -> True)

demandRoots :: Sample -> M.PairT (M.MaybeT Int) (M.MaybeT Int) -> M.PairD (M.MaybeD Int) (M.MaybeD Int)
demandRoots sample (M.PairT first second) = M.PairD
    (demand (leftDemand sample) first) (demand (rightDemand sample) second)
  where
    demand False _ = M.Skip
    demand True root = M.Need (case M.tval root of
        M.NothingT -> M.NothingD
        M.JustT _ -> M.JustD M.Skip)
