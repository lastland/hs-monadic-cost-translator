{-# LANGUAGE ExplicitForAll #-}
module Case (probes) where

import Control.Monad (replicateM)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, demanded, failure, scalarSource, snapshot)

type P a = M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))
type PureP a = (Maybe [a], Maybe [a])
type Result a = M.PairT (P a) (P a)
type PureResult a = (PureP a, PureP a)

generic :: forall a. (Eq a, Num a, G.Scalar a) => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))) (M.PairT (M.MaybeT (L.ListT a)) (M.MaybeT (L.ListT a))))
generic = G.oneM

sourceSum :: Num a => PureResult a -> a
sourceSum (a,b) = sumP a + sumP b

sumP :: Num a => PureP a -> a
sumP (a,b) = sum (fromMaybe [] a) + sum (fromMaybe [] b)

forceResult :: Num a => Result a -> M.MemoristM a
forceResult (M.PairT a b) = (+) <$> (M.force a >>= forceP) <*> (M.force b >>= forceP)

forceP :: Num a => P a -> M.MemoristM a
forceP (M.PairT a b) = (+) <$> (M.force a >>= forceV) <*> (M.force b >>= forceV)
  where
    forceV M.NothingT = pure 0
    forceV (M.JustT list) = M.force list >>= go
    go L.NilT = pure 0
    go (L.ConsT h t) = (+) <$> M.force h <*> (M.force t >>= go)

withInputs :: Sample -> (M.Th Int -> M.Th Int -> M.MemoristM a) -> M.MemoristM a
withInputs sample action = do
    input <- M.lazy (pure (undefined :: Int))
    count <- M.lazy (pure (max 0 (width sample)))
    action input count

probes :: [Probe]
probes =
    [ scalarSource "original/generic-int/full" (sourceSum . Source.one (undefined :: Int) . max 0 . width)
        (\s -> withInputs s generic >>= forceResult)
    , scalarSource "original/generic-integer/full" (fromInteger . sourceSum . Source.one (undefined :: Integer) . max 0 . width) $ \s -> do
        input <- M.lazy (pure (undefined :: Integer))
        count <- M.lazy (pure (max 0 (width s)))
        fromInteger <$> (generic input count >>= forceResult)
    , scalarSource "original/concrete-int/full" (sourceSum . Source.one (undefined :: Int) . max 0 . width)
        (\s -> M.fromPure2 G.oneM (undefined :: Int) (max 0 (width s)) >>= forceResult)
    , scalarSource "original/concrete-integer/full" (fromInteger . sourceSum . Source.one (undefined :: Integer) . max 0 . width)
        (\s -> fromInteger <$> (M.fromPure2 G.oneM (undefined :: Integer) (max 0 (width s)) >>= forceResult))
    , scalarSource "original/repeated-fresh" (\s -> repeats s * sourceSum (Source.one (undefined :: Int) (max 0 (width s)))) $ \s ->
        withInputs s $ \input count -> sum <$> replicateM (repeats s) (generic input count >>= forceResult)
    , scalarSource "original/repeated-cached" (\s -> repeats s * sourceSum (Source.one (undefined :: Int) (max 0 (width s)))) $ \s ->
        withInputs s $ \input count -> do
            cached <- M.lazy (generic input count)
            sum <$> replicateM (repeats s) (M.force cached >>= forceResult)
    , demanded "original/result-root" (const (const True)) (const (const (M.PairD M.Skip M.Skip)))
        (`withInputs` generic)
    , snapshot "original/shared-and-fresh-identities" $ \s -> M.value $ M.eval1 $ withInputs s $ \input count -> do
        first <- generic input count
        second <- generic input count
        a <- names first
        b <- names second
        pure (distinctResultRoots first && distinctResultRoots second &&
            disjoint (resultRootNames first) (resultRootNames second) &&
            validGroups a && validGroups b && disjoint (concat a) (concat b))
    , demanded "original/selected-helper-result-roots" projectHelperRoots demandHelperRoots
        (`withInputs` generic)
    ]
    ++ preservationProbes

projectHelperRoots :: Sample -> Result Int -> (Maybe Bool, Maybe Bool)
projectHelperRoots sample (M.PairT left right) =
    (project (leftDemand sample) left, project (rightDemand sample) right)
  where
    project False _ = Nothing
    project True root = case M.tval root of
        M.PairT _ _ -> Just True

demandHelperRoots :: Sample -> Result Int -> M.PairD (M.PairD (M.MaybeD (L.ListD Int)) (M.MaybeD (L.ListD Int))) (M.PairD (M.MaybeD (L.ListD Int)) (M.MaybeD (L.ListD Int)))
demandHelperRoots sample _ = M.PairD (demand (leftDemand sample)) (demand (rightDemand sample))
  where
    demand wanted = if wanted then M.Need (M.PairD M.Skip M.Skip) else M.Skip

names :: Result a -> M.MemoristM [[M.ThName]]
names (M.PairT a b) = (++) <$> (M.force a >>= namesP) <*> (M.force b >>= namesP)

namesP :: P a -> M.MemoristM [[M.ThName]]
namesP (M.PairT a b) = sequence [namesV a, namesV b]

namesV :: M.Th (M.MaybeT (L.ListT a)) -> M.MemoristM [M.ThName]
namesV root = do
    value <- M.force root
    case value of
        M.NothingT -> error "expected Just"
        M.JustT list -> do
            elements <- M.force list
            case elements of
                L.NilT -> error "expected singleton"
                L.ConsT h t -> pure [M.tname root, M.tname list, M.tname h, M.tname t]

resultRootNames :: Result a -> [M.ThName]
resultRootNames (M.PairT left right) = [M.tname left, M.tname right]

distinctResultRoots :: Result a -> Bool
distinctResultRoots (M.PairT left right) = M.tname left /= M.tname right

disjoint :: Eq a => [a] -> [a] -> Bool
disjoint a b = all (`notElem` b) a

validGroups :: [[M.ThName]] -> Bool
validGroups [a,b,c,d] = length a == 4 && a == b && a == c && a == d
validGroups _ = False

genericOneLoop :: (Eq a, Num a, G.Scalar a) => M.Th a -> M.Th Int -> M.MemoristM (P a)
genericOneLoop = G.oneLoopM

genericTwice, genericCached, genericSeparate, genericBrokenHead, genericBrokenRoot :: (Eq a, Num a, G.Scalar a) => M.Th a -> M.Th Int -> M.MemoristM (Result a)
genericTwice = G.twiceM
genericCached = G.cachedM
genericSeparate = G.separateM
genericBrokenHead = G.brokenHeadM
genericBrokenRoot = G.brokenRootM

genericFrames :: (Eq a, Num a, G.Scalar a) => M.Th a -> M.Th Int -> M.MemoristM (L.ListT (P a))
genericFrames = G.framesM

depth :: Sample -> Int
depth = max 0 . width

forceFrames :: Num a => L.ListT (P a) -> M.MemoristM a
forceFrames L.NilT = pure 0
forceFrames (L.ConsT h t) = (+) <$> (M.force h >>= forceP) <*> (M.force t >>= forceFrames)

preservationProbes :: [Probe]
preservationProbes =
    [ scalarSource "one-loop/generic-int/full" (sumP . Source.oneLoop (undefined :: Int) . depth)
        (\s -> withInputs s genericOneLoop >>= forceP)
    , scalarSource "one-loop/generic-integer/full" (fromInteger . sumP . Source.oneLoop (undefined :: Integer) . depth) $ \s -> do
        input <- M.lazy (pure (undefined :: Integer))
        count <- M.lazy (pure (depth s))
        fromInteger <$> (genericOneLoop input count >>= forceP)
    , scalarSource "twice/full" (sourceSum . Source.twice (undefined :: Int) . depth)
        (\s -> withInputs s genericTwice >>= forceResult)
    , scalarSource "cached-helper/full" (sourceSum . Source.cached (undefined :: Int) . depth)
        (\s -> withInputs s genericCached >>= forceResult)
    , scalarSource "frames/full" (sum . map sumP . Source.frames (undefined :: Int) . depth)
        (\s -> withInputs s genericFrames >>= forceFrames)
    , scalarSource "separate/full" (sourceSum . Source.separate (undefined :: Int) . depth)
        (\s -> withInputs s genericSeparate >>= forceResult)
    , scalarSource "one-loop/repeated-fresh" (\s -> repeats s * sumP (Source.oneLoop (undefined :: Int) (depth s))) $ \s ->
        withInputs s $ \input count -> sum <$> replicateM (repeats s) (genericOneLoop input count >>= forceP)
    , scalarSource "one-loop/repeated-cached" (\s -> repeats s * sumP (Source.oneLoop (undefined :: Int) (depth s))) $ \s ->
        withInputs s $ \input count -> do
            cached <- M.lazy (genericOneLoop input count)
            sum <$> replicateM (repeats s) (M.force cached >>= forceP)
    , scalarSource "twice/repeated-fresh" (\s -> repeats s * sourceSum (Source.twice (undefined :: Int) (depth s))) $ \s ->
        withInputs s $ \input count -> sum <$> replicateM (repeats s) (genericTwice input count >>= forceResult)
    , scalarSource "twice/repeated-cached" (\s -> repeats s * sourceSum (Source.twice (undefined :: Int) (depth s))) $ \s ->
        withInputs s $ \input count -> do
            cached <- M.lazy (genericTwice input count)
            sum <$> replicateM (repeats s) (M.force cached >>= forceResult)
    , demanded "twice/selected-helper-roots" projectHelperRoots demandHelperRoots (`withInputs` genericTwice)
    , demanded "broken-root/helper-roots" (projectHelperRoots . both) (demandHelperRoots . both) (`withInputs` genericBrokenRoot)
    , demanded "broken-head/full-list-spines" projectSpines demandSpines (`withInputs` genericBrokenHead)
    , failure "broken-head/full" (\s -> withInputs s genericBrokenHead >>= forceResult)
    , failure "broken-root/full" (\s -> withInputs s genericBrokenRoot >>= forceResult)
    , identities "twice/shared-vectors" $ \s -> do
        result <- withInputs s genericTwice
        vectors <- names result
        pure (distinctResultRoots result && validGroups vectors)
    , identities "cached-helper/aliased-result" $ \s -> do
        result@(M.PairT a b) <- withInputs s genericCached
        vectors <- names result
        pure (M.tname a == M.tname b && validGroups vectors)
    , identities "twice/fresh-outers-disjoint" $ \s -> withInputs s $ \input count -> do
        first <- genericTwice input count >>= names
        second <- genericTwice input count >>= names
        pure (validGroups first && validGroups second && disjoint (concat first) (concat second))
    , identities "separate/bindings-disjoint" $ \s -> do
        vectors <- withInputs s genericSeparate >>= names
        pure (case vectors of
            [a,b,c,d] -> length a == 4 && length b == 4 && a == c && b == d && disjoint a b
            _ -> False)
    , identities "frames/all-recursive-frames-alias" $ \s -> do
        entries <- withInputs s genericFrames >>= namesFrames
        pure (length entries == depth s + 1 && case entries of
            [] -> False
            (_,a,_):_ -> all (\(_,b,c) -> a == b && a == c) entries &&
                length (nub [n | (n,_,_) <- entries]) == length entries)
    , identities "frames/fresh-outers-disjoint" $ \s -> withInputs s $ \input count -> do
        first <- genericFrames input count >>= namesFrames
        second <- genericFrames input count >>= namesFrames
        pure (case (first,second) of
            ((_,a,_):_,(_,b,_):_) -> length a == 4 && length b == 4 && disjoint a b
            _ -> False)
    ]
  where
    identities label program = snapshot label (M.value . M.eval1 . program)
    both s = s { leftDemand = True, rightDemand = True }

projectSpines :: Sample -> Result Int -> ((Int,Int),(Int,Int))
projectSpines _ (M.PairT a b) = (viewP (M.tval a),viewP (M.tval b))
  where
    viewP (M.PairT c d) = (viewV (M.tval c),viewV (M.tval d))
    viewV M.NothingT = 0
    viewV (M.JustT t) = lengthL (M.tval t)
    lengthL L.NilT = 0
    lengthL (L.ConsT _ t) = 1 + lengthL (M.tval t)

demandSpines :: Sample -> Result Int -> M.PairD (M.PairD (M.MaybeD (L.ListD Int)) (M.MaybeD (L.ListD Int))) (M.PairD (M.MaybeD (L.ListD Int)) (M.MaybeD (L.ListD Int)))
demandSpines _ _ = M.PairD (M.Need pair) (M.Need pair)
  where
    pair = M.PairD payload payload
    payload = M.Need (M.JustD (M.Need (L.ConsD M.Skip (M.Need L.NilD))))

namesFrames :: L.ListT (P a) -> M.MemoristM [(M.ThName,[M.ThName],[M.ThName])]
namesFrames L.NilT = pure []
namesFrames (L.ConsT h t) = do
    M.PairT a b <- M.force h
    left <- namesV a
    right <- namesV b
    rest <- M.force t >>= namesFrames
    pure ((M.tname h,left,right):rest)
