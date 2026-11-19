module Case (probes) where

import Control.Monad (replicateM, replicateM_, (>=>))
import qualified Example as Source
import Generated (viewsM)
import qualified Memorist.Base.Thunk as MT
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport

type Views = M.PairT
    (M.PairT (M.MaybeT (L.ListT Int)) (M.MaybeT (L.ListT Int)))
    (M.MaybeT (M.MaybeT Int))

probes :: [Probe]
probes =
    [ scalarSource "all-dictionaries/source-value"
        (sourceValue . safeCount) (callViews 0 . safeCount >=> forceAll)
    , demanded "selected-dictionaries" projectViews demandViews
        (callViews 5 . safeCount)
    , demanded "one-alias-per-dictionary" projectViews demandSingle
        (callViews 5 . safeCount)
    , demanded "skip-bottom-count" (\_ _ -> ()) (\_ _ -> M.PairD M.Skip M.Skip)
        (const (callViews 5 undefined))
    , demanded "outer-pair-only/bottom-count" (\_ _ -> ())
        (\_ _ -> M.PairD (M.Need (M.PairD M.Skip M.Skip)) M.Skip)
        (const (callViews 5 undefined))
    , demanded "skip-negative-count" (\_ _ -> ()) (\_ _ -> M.PairD M.Skip M.Skip)
        (const (callViews 5 (-1)))
    , scalarSource "repeated-demands/same-result"
        (\s -> repeats s * sourceValue (safeCount s)) $ \s -> do
        result <- callViews 5 (safeCount s)
        sum <$> replicateM (repeats s) (forceAll result)
    , scalarSource "cached-call/repeated-demands"
        (\s -> repeats s * sourceValue (safeCount s)) $ \s -> do
        cached <- M.lazy (callViews 5 (safeCount s))
        sum <$> replicateM (repeats s) (M.force cached >>= forceAll)
    , scalarSource "fresh-calls/shared-count"
        (\s -> repeats s * sourceValue (safeCount s)) $ \s -> do
        count <- paidCount 5 (safeCount s)
        sum <$> replicateM (repeats s) (viewsM count >>= forceAll)
    , failure "demanded-bottom/list-dictionary"
        (const (callViews 0 undefined >>= forceList))
    , failure "demanded-negative/maybe-dictionary"
        (const (callViews 0 (-1) >>= forceMaybe))
    , snapshot "relative-dictionary-identities" $ \s ->
        let [a,b,c] = M.value (M.eval1 (callViews 0 (safeCount s) >>= rootNames))
        in (a == b, a /= c, b /= c)
    , snapshot "fresh-call-identities" $ \s -> M.value $ M.eval1 $ do
        count <- paidCount 0 (safeCount s)
        first <- viewsM count >>= rootNames
        second <- viewsM count >>= rootNames
        pure (all (`notElem` second) first)
    -- views costs 1; each demanded dictionary costs n+1 producer entries.
    -- The five-tick count is paid once, even when both dictionaries read it.
    , snapshot "worked-cost/all-roots/zero" (const (costIs 8 (callViews 5 0 >>= forceAll)))
    , snapshot "worked-cost/all-roots/two" (const (costIs 12 (callViews 5 2 >>= forceAll)))
    , snapshot "worked-cost/one-root/two" (const (costIs 9 (callViews 5 2 >>= forceList)))
    , snapshot "worked-cost/roots-only/bottom-count"
        (const (costIs 1 (callViews 5 undefined >>= rootNames >> pure (0 :: Int))))
    ]

-- This is the ordinary translated caller: no reflection, setup or new constraint.
callViews :: Int -> Int -> M.MemoristM Views
callViews ticks count = paidCount ticks count >>= viewsM

paidCount :: Int -> Int -> M.MemoristM (M.Th Int)
paidCount ticks count = M.lazy (replicateM_ ticks M.tick >> pure count)

safeCount :: Sample -> Int
safeCount = max 0 . number

sourceValue :: Int -> Int
sourceValue n = case Source.views n of
    ((a,b),c) -> absent a + absent b + absent c

absent :: Maybe a -> Int
absent Nothing = 1
absent (Just _) = error "unexpected payload"

forceAll :: Views -> M.MemoristM Int
forceAll result@(M.PairT aliases _) = do
    M.PairT _ second <- M.force aliases
    a <- forceList result
    b <- M.force second >>= absentT
    c <- forceMaybe result
    pure (a + b + c)

forceList :: Views -> M.MemoristM Int
forceList (M.PairT aliases _) = do
    M.PairT first _ <- M.force aliases
    M.force first >>= absentT

forceMaybe :: Views -> M.MemoristM Int
forceMaybe (M.PairT _ root) = M.force root >>= absentT

absentT :: M.MaybeT a -> M.MemoristM Int
absentT M.NothingT = pure 1
absentT (M.JustT _) = error "unexpected payload"

projectViews :: Sample -> Views -> (Maybe (Int, Int), Maybe Int)
projectViews s (M.PairT aliases root) =
    (if leftDemand s then case M.tval aliases of
        M.PairT first second -> Just (tag (M.tval first), tag (M.tval second))
     else Nothing,
     if rightDemand s then Just (tag (M.tval root)) else Nothing)
  where
    tag M.NothingT = 1
    tag (M.JustT _) = error "unexpected payload"

demandViews :: Sample -> Views -> M.PairD
    (M.PairD (M.MaybeD (L.ListD Int)) (M.MaybeD (L.ListD Int)))
    (M.MaybeD (M.MaybeD Int))
demandViews s (M.PairT aliases root) = M.PairD
    (if leftDemand s then M.fullDemand aliases else M.Skip)
    (if rightDemand s then M.fullDemand root else M.Skip)

demandSingle :: Sample -> Views -> M.PairD
    (M.PairD (M.MaybeD (L.ListD Int)) (M.MaybeD (L.ListD Int)))
    (M.MaybeD (M.MaybeD Int))
demandSingle s (M.PairT aliases root) = M.PairD
    (if leftDemand s then case M.tval aliases of
        M.PairT _ second -> M.Need (M.PairD M.Skip (M.fullDemand second))
     else M.Skip)
    (if rightDemand s then M.fullDemand root else M.Skip)

rootNames :: Views -> M.MemoristM [M.ThName]
rootNames (M.PairT aliases root) = do
    M.PairT first second <- M.force aliases
    pure [M.tname first, M.tname second, M.tname root]

costIs :: M.Demand a => Integer -> M.MemoristM a -> Bool
costIs expected program =
    let actual = MT.toInteger (M.infcost program M.fullDemand)
    in if actual == expected then True else error ("wrong worked cost: " ++ show actual)
