module Case (probes) where

import Control.Monad (replicateM)
import Data.Maybe (fromMaybe)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, failure, scalarSource, snapshot)

type Views = M.PairT (M.MaybeT Int) (M.MaybeT Int)

probes :: [Probe]
probes =
    [ scalarSource "full/total-count" (sourceSum True True . number) $ \s ->
        call (number s) >>= forceCopies True True
    , scalarSource "full/zero" (const (sourceSum True True 0)) $ \_ ->
        call 0 >>= forceCopies True True
    , scalarSource "full/min-bound" (const (sourceSum True True minBound)) $ \_ ->
        call minBound >>= forceCopies True True
    , scalarSource "full/max-bound" (const (sourceSum True True maxBound)) $ \_ ->
        call maxBound >>= forceCopies True True
    , scalarSource "left/right-skipped" (sourceSum True False . number) $ \s ->
        call (number s) >>= forceCopies True False
    , scalarSource "right/left-skipped" (sourceSum False True . number) $ \s ->
        call (number s) >>= forceCopies False True
    , demanded "full/demand" (const pureSum) (const M.fullDemand) (call . number)
    , demanded "selected/payloads" projectPayloads demandPayloads (call . number)
    , demanded "selected/roots-paid-count-skipped" projectRoots demandRoots (paidCount . number)
    , demanded "skip-all/paid-count-skipped" (\_ _ -> ()) (\_ _ -> M.PairD M.Skip M.Skip)
        (paidCount . number)
    , demanded "skip-all/bottom-count" (\_ _ -> ()) (\_ _ -> M.PairD M.Skip M.Skip)
        (const (call undefined))
    , demanded "roots-only/bottom-count" (\_ _ -> ()) (\_ _ -> rootDemand)
        (const (call undefined))
    , failure "payload/bottom-count" $ \_ -> call undefined >>= forceCopies True True
    , scalarSource "full/paid-count" (sourceSum True True . number) $ \s ->
        paidCount (number s) >>= forceCopies True True
    , scalarSource "four-fresh" (\s -> 4 * sourceSum True True (number s)) $ \s ->
        sum <$> replicateM 4 (call (number s) >>= forceCopies True True)
    , scalarSource "four-cached" (\s -> 4 * sourceSum True True (number s)) $ \s -> do
        cached <- M.lazy (call (number s))
        sum <$> replicateM 4 (M.force cached >>= forceCopies True True)
    , scalarSource "repeated/fresh" repeatedSource $ \s -> do
        count <- M.lazy (pure (number s))
        sum <$> replicateM (repeats s) (generic count >>= forceCopies True True)
    , scalarSource "repeated/cached" repeatedSource $ \s -> do
        cached <- M.lazy (call (number s))
        sum <$> replicateM (repeats s) (M.force cached >>= forceCopies True True)
    , scalarSource "repeated/fresh-paid-count" repeatedSource $ \s -> do
        count <- M.lazy (M.tick >> pure (number s))
        sum <$> replicateM (repeats s) (generic count >>= forceCopies True True)
    , snapshot "identity/cross-view-root-and-payload-aliases" $ \s -> M.value $ M.eval1 $ do
        groups <- call (number s) >>= names
        pure (valid groups)
    , snapshot "identity/fresh-invocations" $ \s -> M.value $ M.eval1 $ do
        count <- M.lazy (pure (number s))
        first <- generic count >>= names
        second <- generic count >>= names
        pure (valid first && valid second && all (`notElem` concat second) (concat first))
    ]

-- The ordinary caller needs no reflection, setup, or function bundle.
generic :: M.Th Int -> M.MemoristM Views
generic = G.viewsM

call :: Int -> M.MemoristM Views
call n = M.lazy (pure n) >>= generic

paidCount :: Int -> M.MemoristM Views
paidCount n = M.lazy (M.tick >> pure n) >>= generic

repeatedSource :: Sample -> Int
repeatedSource s = repeats s * sourceSum True True (number s)

sourceSum :: Bool -> Bool -> Int -> Int
sourceSum left right n = selected left first + selected right second
  where
    (first, second) = Source.views n
    selected wanted value = if wanted then fromMaybe 0 value else 0

forceCopies :: Bool -> Bool -> Views -> M.MemoristM Int
forceCopies left right (M.PairT first second) = do
    a <- selected left first
    b <- selected right second
    pure (a+b)
  where
    selected False _ = pure 0
    selected True root = M.force root >>= payload
    payload M.NothingT = pure 0
    payload (M.JustT value) = M.force value

pureSum :: Views -> Int
pureSum (M.PairT first second) = payload (M.tval first) + payload (M.tval second)
  where
    payload M.NothingT = 0
    payload (M.JustT value) = M.tval value

projectPayloads :: Sample -> Views -> (Maybe (Maybe Int), Maybe (Maybe Int))
projectPayloads s (M.PairT first second) =
    (selected (leftDemand s) first, selected (rightDemand s) second)
  where
    selected False _ = Nothing
    selected True root = Just (case M.tval root of
        M.NothingT -> Nothing
        M.JustT value -> Just (M.tval value))

demandPayloads :: Sample -> Views -> M.PairD (M.MaybeD Int) (M.MaybeD Int)
demandPayloads s (M.PairT first second) = M.PairD
    (if leftDemand s then M.fullDemand first else M.Skip)
    (if rightDemand s then M.fullDemand second else M.Skip)

projectRoots :: Sample -> Views -> (Maybe Bool, Maybe Bool)
projectRoots s (M.PairT first second) =
    (selected (leftDemand s) first, selected (rightDemand s) second)
  where
    selected False _ = Nothing
    selected True root = Just (case M.tval root of
        M.NothingT -> False
        M.JustT _ -> True)

demandRoots :: Sample -> Views -> M.PairD (M.MaybeD Int) (M.MaybeD Int)
demandRoots s _ = M.PairD
    (if leftDemand s then root else M.Skip)
    (if rightDemand s then root else M.Skip)
  where root = M.Need (M.JustD M.Skip)

rootDemand :: M.PairD (M.MaybeD Int) (M.MaybeD Int)
rootDemand = M.PairD root root
  where root = M.Need (M.JustD M.Skip)

names :: Views -> M.MemoristM [[M.ThName]]
names (M.PairT first second) = sequence [group first, group second]
  where
    group root = do
        value <- M.force root
        pure (M.tname root : case value of
            M.NothingT -> []
            M.JustT payload -> [M.tname payload])

valid :: [[M.ThName]] -> Bool
valid [[first,payload],[second,other]] = first == second && payload == other && first /= payload
valid _ = False
