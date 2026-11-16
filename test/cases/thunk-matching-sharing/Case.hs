{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Case (probes) where

import Control.Monad (replicateM)
import Data.List (elemIndex)
import qualified Example as Source
import Generated
import qualified Memorist.Memorist as M
import ReferenceSupport

-- Keep the catalog static: every sample runs every semantic family.
probes :: [Probe]
probes =
    [ scalarSource "poly/top-repeated" topSource $ \s -> do
        pairs <- topPairs s
        totals <- replicateM (repeats s) $ mapM observeTop pairs
        pure (sum (concat totals))
    , scalarSource "poly/local-repeated" localSource $ \s -> do
        pairs <- localPairs s
        totals <- replicateM (repeats s) $ mapM observeLocal pairs
        pure (sum (concat totals))
    , demanded "poly/top-case/partial"
        (projectPair (\x -> M.abstract x :: [Int]) (\x -> M.abstract x :: [Int])) pairDemand $ \s -> do
        bindings <- initialize
        M.fromPure1 (polyCaseM bindings) (number s)
    , demanded "poly/top-guard/partial"
        (projectPair (\x -> M.abstract x :: [Bool]) (\x -> M.abstract x :: [Bool])) pairDemand $ \s -> do
        bindings <- initialize
        M.fromPure1 (polyGuardM bindings) (other s)
    , demanded "poly/local-case/partial"
        (projectPair (\x -> M.abstract x :: [Int]) (\x -> M.abstract x :: [Bool])) pairDemand $ \_ -> do
        bindings <- initialize
        M.fromPure1 (localPolyCaseM bindings) (undefined :: Int)
    , demanded "poly/local-guard/partial"
        (projectPair (\x -> M.abstract x :: [Int]) (\x -> M.abstract x :: [Bool])) pairDemand $ \_ -> do
        bindings <- initialize
        M.fromPure1 (localPolyGuardM bindings) (undefined :: Bool)
    , snapshot "identities/top-polymorphic" $ \s ->
        names (concatMap pairNames (M.value (M.eval1 (topPairs s))))
    , snapshot "identities/local-polymorphic-freshness" $ \s ->
        names (concatMap pairNames (M.value (M.eval1 (localPairs s))))
    , scalarSource "numeric/case" (\s -> sumSourcePair id id (Source.sameCase (number s))) $ \s -> do
        bindings <- initialize
        M.fromPure1 (sameCaseM bindings) (number s) >>= observeBoth id id
    , scalarSource "numeric/guard" (\s -> sumSourcePair id id (Source.sameGuard (number s))) $ \s -> do
        bindings <- initialize
        M.fromPure1 (sameGuardM bindings) (number s) >>= observeBoth id id
    , scalarSource "numeric/different-dictionaries" (\s -> sumSourcePair id fromInteger (Source.different (number s))) $ \s -> do
        bindings <- initialize
        M.fromPure1 (differentM bindings) (number s) >>= observeBoth id fromInteger
    , scalarSource "numeric/scoped-case-int" (\s -> sumSourcePair id id (Source.scopedCase (number s))) $ \s -> do
        bindings <- initialize
        M.fromPure1 (scopedCaseM bindings) (number s) >>= observeBoth id id
    , scalarSource "numeric/scoped-case-integer" (\s -> sumSourcePair fromInteger fromInteger (Source.scopedCase (toInteger (other s)))) $ \s -> do
        bindings <- initialize
        M.fromPure1 (scopedCaseM bindings) (toInteger (other s)) >>= observeBoth fromInteger fromInteger
    , scalarSource "numeric/scoped-guard-int" (\s -> sumSourcePair id id (Source.scopedGuard (number s))) $ \s -> do
        bindings <- initialize
        M.fromPure1 (scopedGuardM bindings) (number s) >>= observeBoth id id
    , scalarSource "numeric/scoped-guard-integer" (\s -> sumSourcePair fromInteger fromInteger (Source.scopedGuard (toInteger (other s)))) $ \s -> do
        bindings <- initialize
        M.fromPure1 (scopedGuardM bindings) (toInteger (other s)) >>= observeBoth fromInteger fromInteger
    , scalarSource "numeric/local-scoped-int" (\s -> sumSourcePair id id (Source.localScoped (number s))) $ \s -> do
        bindings <- initialize
        M.fromPure1 (localScopedM bindings) (number s) >>= observeBoth id id
    , scalarSource "numeric/local-scoped-integer" (\s -> sumSourcePair fromInteger fromInteger (Source.localScoped (toInteger (other s)))) $ \s -> do
        bindings <- initialize
        M.fromPure1 (localScopedM bindings) (toInteger (other s)) >>= observeBoth fromInteger fromInteger
    , scalarSource "numeric/repeated-observations" numericSource $ \s -> do
        (a,b,c,d,e,f) <- numericPairs s
        totals <- replicateM (repeats s) $ do
            av <- observeBoth id id a
            bv <- observeBoth id id b
            cv <- observeBoth id fromInteger c
            dv <- observeBoth id id d
            ev <- observeBoth fromInteger fromInteger e
            fv <- observeBoth id id f
            pure (sum [av,bv,cv,dv,ev,fv])
        pure (sum totals)
    , integer "numeric/selective-repeated-observations" $ \s -> do
        (a,b,c,d,e,f) <- numericPairs s
        totals <- replicateM (repeats s) $ do
            av <- observeSelected s toInteger toInteger a
            bv <- observeSelected s toInteger toInteger b
            cv <- observeSelected s toInteger id c
            dv <- observeSelected s toInteger toInteger d
            ev <- observeSelected s id id e
            fv <- observeSelected s toInteger toInteger f
            pure (sum [av,bv,cv,dv,ev,fv])
        pure (sum totals)
    , demanded "numeric/case/partial" (projectPair id id) pairDemand $ \s -> do
        bindings <- initialize
        M.fromPure1 (sameCaseM bindings) (number s)
    , demanded "numeric/guard/partial" (projectPair id id) pairDemand $ \s -> do
        bindings <- initialize
        M.fromPure1 (sameGuardM bindings) (number s)
    , demanded "numeric/different-dictionaries/partial" (projectPair id id) pairDemand $ \s -> do
        bindings <- initialize
        M.fromPure1 (differentM bindings) (number s)
    , demanded "numeric/scoped-case-int/partial" (projectPair id id) pairDemand $ \s -> do
        bindings <- initialize
        M.fromPure1 (scopedCaseM bindings) (number s)
    , demanded "numeric/scoped-case-integer/partial" (projectPair id id) pairDemand $ \s -> do
        bindings <- initialize
        M.fromPure1 (scopedCaseM bindings) (toInteger (other s))
    , demanded "numeric/scoped-guard-int/partial" (projectPair id id) pairDemand $ \s -> do
        bindings <- initialize
        M.fromPure1 (scopedGuardM bindings) (number s)
    , demanded "numeric/scoped-guard-integer/partial" (projectPair id id) pairDemand $ \s -> do
        bindings <- initialize
        M.fromPure1 (scopedGuardM bindings) (toInteger (other s))
    , demanded "numeric/local-scoped-int/partial" (projectPair id id) pairDemand $ \s -> do
        bindings <- initialize
        M.fromPure1 (localScopedM bindings) (number s)
    , demanded "numeric/local-scoped-integer/partial" (projectPair id id) pairDemand $ \s -> do
        bindings <- initialize
        M.fromPure1 (localScopedM bindings) (toInteger (other s))
    , snapshot "identities/dictionaries" $ \s ->
        names (numericNames (M.value (M.eval1 (numericPairs s))))
    , scalarSource "wide/bool" (\s -> sumSourcePair fst fst (Source.wideBool (number s))) $ \s -> do
        bindings <- initialize
        M.fromPure1 (wideBoolM bindings) (number s) >>= observeWide id
    , scalarSource "wide/int" (\s -> sumSourcePair fst fst (Source.wideInt (number s))) $ \s -> do
        bindings <- initialize
        M.fromPure1 (wideIntM bindings) (number s) >>= observeWide id
    , scalarSource "wide/integer" (\s -> sumSourcePair (fromInteger . fst) (fromInteger . fst) (Source.wideInteger (number s))) $ \s -> do
        bindings <- initialize
        M.fromPure1 (wideIntegerM bindings) (number s) >>= observeWide fromInteger
    , scalarSource "wide/repeated-observations" wideSource $ \s -> do
        (a,b,c) <- widePairs s
        totals <- replicateM (repeats s) $ do
            av <- observeWide id a
            bv <- observeWide id b
            cv <- observeWide fromInteger c
            pure (av + bv + cv)
        pure (sum totals)
    , demanded "wide/bool/partial"
        (projectPair (\x -> M.abstract x :: (Int,[Bool])) (\x -> M.abstract x :: (Int,[Bool]))) pairDemand $ \s -> do
        bindings <- initialize
        M.fromPure1 (wideBoolM bindings) (number s)
    , demanded "wide/int/partial"
        (projectPair (\x -> M.abstract x :: (Int,[Int])) (\x -> M.abstract x :: (Int,[Int]))) pairDemand $ \s -> do
        bindings <- initialize
        M.fromPure1 (wideIntM bindings) (number s)
    , demanded "wide/integer/partial"
        (projectPair (\x -> M.abstract x :: (Integer,[Bool])) (\x -> M.abstract x :: (Integer,[Bool]))) pairDemand $ \s -> do
        bindings <- initialize
        M.fromPure1 (wideIntegerM bindings) (number s)
    , snapshot "identities/wide-dictionary-only-keys" $ \s ->
        names (wideNames (M.value (M.eval1 (widePairs s))))
    , scalarSource "ignored/concrete" (\s -> Source.ignoredConcrete (number s)) $ \s -> do
        bindings <- initialize
        M.fromPure1 (ignoredConcreteM bindings) (number s)
    , scalarSource "ignored/scoped-int" (\s -> Source.ignoredScoped (number s)) $ \s -> do
        bindings <- initialize
        M.fromPure1 (ignoredScopedM bindings) (number s)
    , scalarSource "ignored/scoped-integer" (\s -> fromInteger (Source.ignoredScoped (toInteger (other s)))) $ \s -> do
        bindings <- initialize
        fromInteger <$> M.fromPure1 (ignoredScopedM bindings) (toInteger (other s))
    , scalarSource "ignored/defaulted" (\s -> Source.ignoredDefault (number s)) $ \s -> do
        bindings <- initialize
        M.fromPure1 (ignoredDefaultM bindings) (number s)
    , scalarSource "ignored/local-scoped-int" (\s -> Source.ignoredLocalScoped (number s)) $ \s -> do
        bindings <- initialize
        M.fromPure1 (ignoredLocalScopedM bindings) (number s)
    , scalarSource "ignored/local-scoped-integer" (\s -> fromInteger (Source.ignoredLocalScoped (toInteger (other s)))) $ \s -> do
        bindings <- initialize
        fromInteger <$> M.fromPure1 (ignoredLocalScopedM bindings) (toInteger (other s))
    , scalarSource "defaulted/case" (\s -> Source.defaultedCase (number s)) $ \s -> do
        bindings <- initialize
        M.fromPure1 (defaultedCaseM bindings) (number s)
    , scalarSource "defaulted/guard" (\s -> Source.defaultedGuard (number s)) $ \s -> do
        bindings <- initialize
        M.fromPure1 (defaultedGuardM bindings) (number s)
    , scalarSource "defaulted/repeated-shared-dictionary"
        (\s -> sum (replicate (repeats s) (Source.defaultedCase (number s) + Source.defaultedGuard (number s)))) $ \s -> do
        bindings <- initialize
        argument <- M.lazy (pure (number s))
        results <- replicateM (repeats s) $ do
            a <- defaultedCaseM bindings argument
            b <- defaultedGuardM bindings argument
            pure (a + b)
        pure (sum results)
    , scalarSource "bottom/local-case/source" (\s -> fst (Source.bottomCase (number s))) $ \s ->
        bottomCaseProgram s >>= firstField
    , demanded "bottom/local-case/suspended" firstProjection firstDemand bottomCaseProgram
    , failure "bottom/local-case/demanded" $ \s ->
        bottomCaseProgram s >>= secondField
    , scalarSource "bottom/local-guard/source" (\s -> fst (Source.bottomGuard (number s))) $ \s ->
        bottomGuardProgram s >>= firstField
    , demanded "bottom/local-guard/suspended" firstProjection firstDemand bottomGuardProgram
    , failure "bottom/local-guard/demanded" $ \s ->
        bottomGuardProgram s >>= secondField
    , scalarSource "bottom/computed-case/source" (\s -> fst (Source.computedBottomCase (number s))) $ \s ->
        computedBottomCaseProgram s >>= firstField
    , demanded "bottom/computed-case/suspended" firstProjection firstDemand computedBottomCaseProgram
    , failure "bottom/computed-case/demanded" $ \s ->
        computedBottomCaseProgram s >>= secondField
    , scalarSource "bottom/computed-guard/source" (\s -> fst (Source.computedBottomGuard (number s))) $ \s ->
        computedBottomGuardProgram s >>= firstField
    , demanded "bottom/computed-guard/suspended" firstProjection firstDemand computedBottomGuardProgram
    , failure "bottom/computed-guard/demanded" $ \s ->
        computedBottomGuardProgram s >>= secondField
    ]

projectPair :: (a -> x) -> (b -> y) -> Sample -> M.PairT a b -> (Maybe x,Maybe y)
projectPair projectLeft projectRight s (M.PairT a b) =
    (if leftDemand s then Just (projectLeft (M.tval a)) else Nothing,
     if rightDemand s then Just (projectRight (M.tval b)) else Nothing)

pairDemand :: (M.Demand a, M.Demand b) => Sample -> M.PairT a b -> M.PairD (M.Dem a) (M.Dem b)
pairDemand s (M.PairT a b) = M.PairD
    (if leftDemand s then M.fullDemand a else M.Skip)
    (if rightDemand s then M.fullDemand b else M.Skip)

firstProjection :: Sample -> M.PairT Int Int -> Int
firstProjection _ (M.PairT a _) = M.tval a

firstDemand :: Sample -> M.PairT Int Int -> M.PairD Int Int
firstDemand _ (M.PairT a _) = M.PairD (M.fullDemand a) M.Skip

bottomCaseProgram s = do
    bindings <- initialize
    M.fromPure1 (bottomCaseM bindings) (number s)

bottomGuardProgram s = do
    bindings <- initialize
    M.fromPure1 (bottomGuardM bindings) (number s)

computedBottomCaseProgram s = do
    bindings <- initialize
    M.fromPure1 (computedBottomCaseM bindings) (number s)

computedBottomGuardProgram s = do
    bindings <- initialize
    M.fromPure1 (computedBottomGuardM bindings) (number s)

firstField (M.PairT a _) = M.force a
secondField (M.PairT _ b) = M.force b

sumSourcePair left right (a,b) = left a + right b

observeBoth left right (M.PairT a b) = do
    av <- left <$> M.force a
    bv <- right <$> M.force b
    pure (av + bv)

observeSelected s left right (M.PairT a b) = do
    av <- if leftDemand s then left <$> M.force a else pure 0
    bv <- if rightDemand s then right <$> M.force b else pure 0
    pure (av + bv)

observeWide convert (M.PairT a b) = do
    M.PairT av _ <- M.force a
    M.PairT bv _ <- M.force b
    x <- M.force av
    y <- M.force bv
    pure (convert x + convert y)

topPairs s = do
    bindings <- initialize
    argument <- M.lazy (pure ())
    replicateM (repeats s) $ do
        a <- polyCaseM bindings argument
        b <- polyGuardM bindings argument
        pure (a,b)

localPairs s = do
    bindings <- initialize
    argument <- M.lazy (pure ())
    replicateM (repeats s) $ do
        a <- localPolyCaseM bindings argument
        b <- localPolyGuardM bindings argument
        pure (a,b)

observeTop (M.PairT a b,M.PairT c d) = do
    av <- M.force a
    bv <- M.force b
    cv <- M.force c
    dv <- M.force d
    pure (sum [length (M.abstract av :: [Int]), length (M.abstract bv :: [Int]),
               length (M.abstract cv :: [Bool]), length (M.abstract dv :: [Bool])])

observeLocal (M.PairT a b,M.PairT c d) = do
    av <- M.force a
    bv <- M.force b
    cv <- M.force c
    dv <- M.force d
    pure (sum [length (M.abstract av :: [Int]), length (M.abstract bv :: [Bool]),
               length (M.abstract cv :: [Int]), length (M.abstract dv :: [Bool])])

listPairLength (a,b) = length a + length b

topSource s = sum $ replicate (repeats s) $ sum $ replicate (repeats s) $
    listPairLength (Source.polyCase ()) + listPairLength (Source.polyGuard ())

localSource s = sum $ replicate (repeats s) $ sum $ replicate (repeats s) $
    listPairLength (Source.localPolyCase ()) + listPairLength (Source.localPolyGuard ())

numericPairs s = do
    bindings <- initialize
    unitArgument <- M.lazy (pure ())
    argument <- M.lazy (pure (number s))
    integerArgument <- M.lazy (pure (toInteger (other s)))
    a <- sameCaseM bindings unitArgument
    b <- sameGuardM bindings unitArgument
    c <- differentM bindings unitArgument
    d <- scopedCaseM bindings argument
    e <- scopedGuardM bindings integerArgument
    f <- localScopedM bindings argument
    pure (a,b,c,d,e,f)

numericSource s = sum $ replicate (repeats s) $ sum
    [ sumSourcePair id id (Source.sameCase ())
    , sumSourcePair id id (Source.sameGuard ())
    , sumSourcePair id fromInteger (Source.different ())
    , sumSourcePair id id (Source.scopedCase (number s))
    , sumSourcePair fromInteger fromInteger (Source.scopedGuard (toInteger (other s)))
    , sumSourcePair id id (Source.localScoped (number s))
    ]

widePairs s = do
    bindings <- initialize
    argument <- M.lazy (pure ())
    a <- wideBoolM bindings argument
    b <- wideIntM bindings argument
    c <- wideIntegerM bindings argument
    pure (a,b,c)

wideSource s = sum $ replicate (repeats s) $ sum
    [ sumSourcePair fst fst (Source.wideBool ())
    , sumSourcePair fst fst (Source.wideInt ())
    , sumSourcePair (fromInteger . fst) (fromInteger . fst) (Source.wideInteger ())
    ]

pairNames (M.PairT a b,M.PairT c d) = [M.tname a,M.tname b,M.tname c,M.tname d]
numericNames (M.PairT a b,M.PairT c d,M.PairT e f,M.PairT g h,M.PairT i j,M.PairT k l) =
    map M.tname [a,b,c,d,e,g,h,k,l] ++ map M.tname [f,i,j]
wideNames (M.PairT a b,M.PairT c d,M.PairT e f) =
    [M.tname a,M.tname b,M.tname c,M.tname d,M.tname e,M.tname f]

-- Compare alias classes, not allocator-specific spelling or reservation order.
names :: Eq a => [a] -> [Int]
names = go []
  where
    go _ [] = []
    go seen (name:rest) = case elemIndex name seen of
        Just index -> index : go seen rest
        Nothing -> length seen : go (seen ++ [name]) rest
