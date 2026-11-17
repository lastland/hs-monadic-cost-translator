module Case (probes) where

import qualified Example as Source
import Generated
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "scoped/bool-unused-input" (Source.scoped (undefined :: Bool) . number)
        (M.fromPure2 scopedM (undefined :: Bool) . number)
    , scalarSource "scoped/int-unused-input" (Source.scoped (undefined :: Int) . number)
        (M.fromPure2 scopedM (undefined :: Int) . number)
    , scalarSource "scoped/negative" (const (Source.scoped (undefined :: Bool) (-3)))
        (const (M.fromPure2 scopedM (undefined :: Bool) (-3 :: Int)))
    , scalarSource "scoped/zero" (const (Source.scoped (undefined :: Bool) 0))
        (const (M.fromPure2 scopedM (undefined :: Bool) (0 :: Int)))
    , scalarSource "scoped/positive" (const (Source.scoped (undefined :: Bool) 3))
        (const (M.fromPure2 scopedM (undefined :: Bool) (3 :: Int)))
    , scalarSource "scoped/two-fresh-invocations" ((2 *) . Source.scoped (undefined :: Bool) . number)
        (twiceScoped . number)
    , scalarSource "views/two-spine-traversals" (sourceViews (undefined :: Bool) . number)
        (observeViews . number)
    , scalarSource "views/empty-two-traversals" (const (sourceViews (undefined :: Bool) 0))
        (const (observeViews 0))
    , demanded "views/partial-spines-bottom-elements" projectViews demandViews
        (M.fromPure2 viewsM (undefined :: Bool) . number)
    , demanded "views/bottom-count-skipped" (\_ _ -> ()) (\_ _ -> M.PairD M.Skip M.Skip)
        (const (M.fromPure2 viewsM (undefined :: Bool) (undefined :: Int)))
    , failure "source/demanded-int-head" (pure . sourceIntHead . positiveCount)
    , failure "views/demanded-int-head" (demandIntHead . positiveCount)
    , failure "source/demanded-bool-head" (pure . sourceBoolHead . positiveCount)
    , failure "views/demanded-bool-head" (demandBoolHead . positiveCount)
    , failure "scoped/demanded-bottom-count" (const (M.fromPure2 scopedM (undefined :: Bool) (undefined :: Int)))
    , snapshot "views/same-root-head-tail-identities" (sameIdentities . positiveCount)
    , snapshot "views/fresh-root-head-tail-identities" (freshIdentities . positiveCount)
    ]

positiveCount :: Sample -> Int
positiveCount sample = width sample + 1

twiceScoped :: Int -> M.MemoristM Int
twiceScoped count = do
    input <- M.lazy (pure (undefined :: Bool))
    n <- M.lazy (pure count)
    first <- scopedM input n
    second <- scopedM input n
    pure (first + second)

sourceViews :: a -> Int -> Int
sourceViews input count =
    let (ints, bools) = Source.views input count
    in Source.spine ints + Source.spine bools

observeViews :: Int -> M.MemoristM Int
observeViews count = do
    M.PairT ints bools <- M.fromPure2 viewsM (undefined :: Bool) count
    left <- spineM ints
    right <- spineM bools
    pure (left + right)

projectViews :: Sample -> M.PairT (L.ListT Int) (L.ListT Bool) -> (Maybe Int, Maybe Int)
projectViews sample (M.PairT ints bools) =
    (project (leftDemand sample) ints, project (rightDemand sample) bools)
  where
    project wanted root
        | not wanted || width sample <= 0 = Nothing
        | otherwise = Just (prefix (width sample) (M.tval root))
    prefix _ L.NilT = 0
    prefix depth (L.ConsT _ rest)
        | depth <= 1 = 1
        | otherwise = 1 + prefix (depth - 1) (M.tval rest)

demandViews :: Sample -> M.PairT (L.ListT Int) (L.ListT Bool) -> M.PairD (L.ListD Int) (L.ListD Bool)
demandViews sample (M.PairT ints bools) = M.PairD
    (demand (leftDemand sample) ints) (demand (rightDemand sample) bools)
  where
    demand wanted root
        | not wanted || width sample <= 0 = M.Skip
        | otherwise = M.Need (prefix (width sample) (M.tval root))
    prefix _ L.NilT = L.NilD
    prefix depth (L.ConsT _ rest) = L.ConsD M.Skip
        (if depth <= 1 then M.Skip else M.Need (prefix (depth - 1) (M.tval rest)))

sourceIntHead :: Int -> Int
sourceIntHead count = case Source.views (undefined :: Bool) count of
    ([], _) -> 0
    (element:_, _) -> element

sourceBoolHead :: Int -> Int
sourceBoolHead count = case Source.views (undefined :: Bool) count of
    (_, []) -> 0
    (_, element:_) -> fromEnum element

demandIntHead :: Int -> M.MemoristM Int
demandIntHead count = do
    M.PairT ints _ <- M.fromPure2 viewsM (undefined :: Bool) count
    xs <- M.force ints
    case xs of
        L.NilT -> pure 0
        L.ConsT element _ -> M.force element

demandBoolHead :: Int -> M.MemoristM Int
demandBoolHead count = do
    M.PairT _ bools <- M.fromPure2 viewsM (undefined :: Bool) count
    xs <- M.force bools
    case xs of
        L.NilT -> pure 0
        L.ConsT element _ -> fromEnum <$> M.force element

thunkNames :: M.Th (L.ListT a) -> [M.ThName]
thunkNames root = M.tname root : case M.tval root of
    L.NilT -> []
    L.ConsT element rest -> M.tname element : thunkNames rest

sameIdentities :: Int -> Bool
sameIdentities count =
    case M.value (M.eval1 (M.fromPure2 viewsM (undefined :: Bool) count)) of
        M.PairT ints bools -> thunkNames ints == thunkNames bools

freshIdentities :: Int -> Bool
freshIdentities count =
    let result = M.value $ M.eval1 $ do
            input <- M.lazy (pure (undefined :: Bool))
            n <- M.lazy (pure count)
            first <- viewsM input n
            second <- viewsM input n
            pure (first, second)
    in case result of
        (M.PairT first _, M.PairT second _) ->
            all (`notElem` thunkNames second) (thunkNames first)
