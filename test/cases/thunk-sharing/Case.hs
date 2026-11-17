module Case (probes) where

import Control.Monad (replicateM, replicateM_)
import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import qualified Memorist.Base.Thunk as MT
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "work/source" (Source.work . number) $ \sample ->
        M.fromPure1 workM (number sample)
    , demanded "same/selected" pairProjection pairDemand $ \_ -> do
        u <- M.lazy (error "unused same argument evaluated" :: M.MemoristM Int)
        sameM u
    , scalarSource "same/source" (\sample -> let (a,b) = Source.same (number sample) in a+b) $ \_ -> do
        u <- M.lazy (pure ())
        sameM u >>= forcePair
    , demanded "different/selected" differentProjection differentDemand $ \_ -> do
        u <- M.lazy (error "unused different argument evaluated" :: M.MemoristM Int)
        differentM u
    , scalarSource "different/source"
        (\sample -> let (a,b) = Source.different (number sample) in a + fromInteger b) $ \_ -> do
        (_, M.PairT a b) <- observeShared
        av <- M.force a
        bv <- M.force b
        pure (av + fromInteger bv)
    , demanded "shared/skipped-fields" (const (const ()))
        (const (const (M.PairD M.Skip M.Skip, M.PairD M.Skip M.Skip))) (const observeShared)
    , scalarSource "shared/fixed-repeated"
        (\_ -> let (a,b) = Source.same (); (c,d) = Source.different () in a+b+c+fromInteger d)
        (const (observeShared >>= forceShared))
    , integer "shared/repeated-demands" $ \sample -> do
        pairs <- observeShared
        sum <$> replicateM (repeats sample) (toInteger <$> forceShared pairs)
    , integer "shared/repeated-calls" $ \sample -> do
        u <- M.lazy (pure ())
        let one index = do
                same <- sameM u
                different <- differentM u
                if even (index + variant sample)
                    then toInteger <$> forceShared (same, different)
                    else do
                        let M.PairT _ d = different
                        _ <- M.force d
                        toInteger <$> forceShared (same, different)
        sum <$> mapM one [1 .. repeats sample]
    , snapshot "shared/preparation-resources" (const sharedResources)
    , demanded "local/selected" pairProjection pairDemand localInput
    , scalarSource "local/source"
        (\sample -> let (a,b) = Source.local (number sample) in a+b)
        (\sample -> localInput sample >>= forcePair)
    , scalarSource "local/fixed-repeated"
        (\_ -> let (a,b) = Source.local (5 :: Int) in a+b) $ \_ ->
        M.fromPure1 localM (5 :: Int) >>= forcePair
    , scalar "local/repeated-demands" $ \sample -> do
        pair <- localInput sample
        sum <$> replicateM (repeats sample) (forcePair pair)
    , scalar "local/repeated-calls" $ \sample -> do
        x <- input sample
        sum <$> replicateM (repeats sample) (localM x >>= forcePair)
    , demanded "local/skipped-error" (const (const ())) (const (const (M.PairD M.Skip M.Skip))) $ \_ -> do
        x <- M.lazy (error "unused local input evaluated" :: M.MemoristM Int)
        localM x
    , failure "local/demanded-error" $ \_ -> do
        x <- M.lazy (error "demanded local input")
        M.PairT _ b <- localM x
        M.force b
    , snapshot "local/preparation-resources" localResources
    ]

input :: Sample -> M.MemoristM (M.Th Int)
input sample = M.lazy (replicateM_ (width sample + 1) M.tick >> pure (number sample))

localInput :: Sample -> M.MemoristM (M.PairT Int Int)
localInput sample = do
    x <- input sample
    localM x

observeShared :: M.MemoristM (M.PairT Int Int, M.PairT Int Integer)
observeShared = do
    u <- M.lazy (pure ())
    a <- sameM u
    b <- differentM u
    pure (a,b)

forcePair :: M.PairT Int Int -> M.MemoristM Int
forcePair (M.PairT a b) = (+) <$> M.force a <*> M.force b

forceShared :: (M.PairT Int Int, M.PairT Int Integer) -> M.MemoristM Int
forceShared (M.PairT a b, M.PairT c d) = do
    av <- M.force a
    bv <- M.force b
    cv <- M.force c
    dv <- M.force d
    pure (av + bv + cv + fromInteger dv)

pairProjection :: Sample -> M.PairT Int Int -> (Maybe Int, Maybe Int)
pairProjection sample (M.PairT a b) =
    (if leftDemand sample then Just (M.tval a) else Nothing,
     if rightDemand sample then Just (M.tval b) else Nothing)

pairDemand :: Sample -> M.PairT Int Int -> M.PairD Int Int
pairDemand sample (M.PairT a b) = M.PairD
    (if leftDemand sample then M.fullDemand a else M.Skip)
    (if rightDemand sample then M.fullDemand b else M.Skip)

differentProjection :: Sample -> M.PairT Int Integer -> (Maybe Int, Maybe Integer)
differentProjection sample (M.PairT a b) =
    (if leftDemand sample then Just (M.tval a) else Nothing,
     if rightDemand sample then Just (M.tval b) else Nothing)

differentDemand :: Sample -> M.PairT Int Integer -> M.PairD Int Integer
differentDemand sample (M.PairT a b) = M.PairD
    (if leftDemand sample then M.fullDemand a else M.Skip)
    (if rightDemand sample then M.fullDemand b else M.Skip)

sharedResources :: ([Bool], Integer)
sharedResources =
    let result = M.eval1 observeShared
        (M.PairT a b, M.PairT c d) = M.value result
    in ([M.tname a == M.tname b, M.tname a == M.tname c, M.tname c /= M.tname d],
        MT.toInteger (M.costAnnot result))

localResources :: Sample -> ([Bool], Integer)
localResources sample =
    let result = M.eval1 $ do
            x <- input sample
            first <- localM x
            second <- localM x
            pure (first, second)
        (M.PairT a b, M.PairT c d) = M.value result
    in ([M.tname a == M.tname b, M.tname c == M.tname d, M.tname a /= M.tname c],
        MT.toInteger (M.costAnnot result))
