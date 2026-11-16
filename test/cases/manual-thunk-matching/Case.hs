module Case (probes) where

import Control.Monad (replicateM, replicateM_, (>=>))
import Generated
import qualified Memorist.Base.Thunk as MT
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport
import qualified ThunkMatching as Source

type PairFunction = Bindings -> M.Th Int -> M.MemoristM (M.PairT Int Int)
type PairAction = M.Th Int -> M.MemoristM (M.PairT Int Int)
type ScalarAction = M.Th Int -> M.MemoristM Int

probes :: [Probe]
-- Instantiate the generated post-Bindings forall before storing a catalog
-- callback. The result annotations make that instantiation explicit.
probes = concatMap pairChecks
    [("case/plain", Source.plain, \bindings -> plainM bindings :: PairAction),
     ("case/parenthesized", Source.parenthesized, \bindings -> parenthesizedM bindings :: PairAction),
     ("case/annotated", Source.annotated, \bindings -> annotatedM bindings :: PairAction),
     ("case/as-pattern", Source.asPattern, \bindings -> asPatternM bindings :: PairAction),
     ("guard/plain", Source.guardAlias, \bindings -> guardAliasM bindings :: PairAction),
     ("guard/parenthesized", Source.guardParenthesized, \bindings -> guardParenthesizedM bindings :: PairAction),
     ("guard/annotated", Source.guardAnnotated, \bindings -> guardAnnotatedM bindings :: PairAction),
     ("guard/as-pattern", Source.guardAs, \bindings -> guardAsM bindings :: PairAction),
     ("computed/case", Source.computedCase, \bindings -> computedCaseM bindings :: PairAction),
     ("computed/guard", Source.computedGuard, \bindings -> computedGuardM bindings :: PairAction),
     ("local/computed", Source.local, \bindings -> localM bindings :: PairAction),
     ("local/explicit-anchor", Source.sourceLet, \bindings -> sourceLetM bindings :: PairAction)] ++
    [ scalarSource "ignored/case" (Source.ignoredCase . number)
        (runScalar (\bindings -> ignoredCaseM bindings :: ScalarAction))
    , scalarSource "ignored/guard" (Source.ignoredGuard . number)
        (runScalar (\bindings -> ignoredGuardM bindings :: ScalarAction))
    , scalarSource "failure/unused-subject" (fst . Source.suspendedFailure . number) $ \s -> do
        M.PairT a _ <- runPair suspendedPair s
        M.force a
    , demanded "failure/unused-fields" (const (const ())) (const (const (M.PairD M.Skip M.Skip)))
        (runPair suspendedPair)
    , failure "failure/demanded-subject" $ \s -> do
        M.PairT _ b <- runPair suspendedPair s
        M.force b
    , failure "bad/direct" $ \s -> do
        bindings <- initialize
        M.fromPure1 (badM bindings) (number s)
    , scalarSource "work/value" (Source.work . number)
        (runScalar (\bindings -> workM bindings :: ScalarAction))
    , scalarSource "dictionaries/repeated" numericSource numericProgram
    , demanded "dictionaries/same-selected" projection demand $ \s -> do
        bindings <- initialize
        M.fromPure1 (sameCaseM bindings) (number s)
    , integer "dictionaries/different-selected" $ \s -> do
        bindings <- initialize
        M.PairT a b <- M.fromPure1 (differentM bindings) (number s)
        left <- if leftDemand s then toInteger <$> M.force a else pure 0
        right <- if rightDemand s then M.force b else pure 0
        pure (left + right)
    , snapshot "dictionaries/identities-and-local-freshness" (const numericIdentities)
    , scalarSource "polymorphic/repeated" polymorphicSource polymorphicProgram
    , snapshot "polymorphic/identities-and-local-freshness" (const polymorphicIdentities)
    , scalarSource "empty/ignored-bottom" (const (length (Source.empty (undefined :: Int) :: [Bool]))) $ \_ -> do
        bindings <- initialize
        value <- M.fromPure1 (emptyM bindings) (undefined :: Int)
        lengthList (value :: L.ListT Bool)
    ]

pairChecks :: (String, Int -> (Int, Int), PairFunction) -> [Probe]
pairChecks (label, source, make) =
    [ demanded (label ++ "/selected") projection demand (runPair make)
    , scalarSource (label ++ "/value") (uncurry (+) . source . number) (runPair make >=> sumPair)
    , scalarSource (label ++ "/unused-bottom") (const (case source undefined of (_, _) -> 1)) $ \_ -> do
        bindings <- initialize
        x <- M.lazy (error "unused matching input")
        M.PairT _ _ <- make bindings x
        pure 1
    , scalar (label ++ "/repeated") $ \s -> do
        pair <- runPair make s
        sum <$> replicateM (repeats s) (sumPair pair)
    , failure (label ++ "/demanded-bottom") $ \_ -> do
        bindings <- initialize
        x <- M.lazy (error "demanded matching input")
        M.PairT _ b <- make bindings x
        M.force b
    , snapshot (label ++ "/aliases-and-caller-allocation") (const (pairResources make))
    ]

runPair :: PairFunction -> Sample -> M.MemoristM (M.PairT Int Int)
runPair make s = do
    bindings <- initialize
    x <- M.lazy (replicateM_ (width s + 1) M.tick >> pure (number s))
    make bindings x

suspendedPair :: PairFunction
suspendedPair bindings = suspendedFailureM bindings :: PairAction

runScalar :: (Bindings -> M.Th Int -> M.MemoristM Int) -> Sample -> M.MemoristM Int
runScalar call s = do
    bindings <- initialize
    M.fromPure1 (call bindings) (number s)

sumPair :: M.PairT Int Int -> M.MemoristM Int
sumPair (M.PairT a b) = (+) <$> M.force a <*> M.force b

projection :: Sample -> M.PairT Int Int -> (Maybe Int, Maybe Int)
projection s (M.PairT a b) =
    (if leftDemand s then Just (M.tval a) else Nothing,
     if rightDemand s then Just (M.tval b) else Nothing)

demand :: Sample -> M.PairT Int Int -> M.PairD Int Int
demand s (M.PairT a b) = M.PairD
    (if leftDemand s then M.fullDemand a else M.Skip)
    (if rightDemand s then M.fullDemand b else M.Skip)

pairResources :: PairFunction -> ([Bool], Integer)
pairResources make =
    let (result, next) = M.runM (do
            bindings <- initialize
            x <- M.lazy (error "identity observation forced input")
            M.PairT a b <- make bindings x
            pure (M.tname x, M.tname a, M.tname b)) M.nzero
        (inputName, firstName, secondName) = M.value result
    in ([firstName == inputName, secondName == inputName, firstName == secondName,
         next == M.nnext inputName, next == M.nnext (M.nnext inputName)],
        MT.toInteger (M.costAnnot result))

numericSource :: Sample -> Int
numericSource s = repeats s *
    (uncurry (+) (Source.sameCase ()) + uncurry (+) (Source.sameGuard ()) +
     (let (a, b) = Source.different () in a + fromInteger b) +
     (let (a, (b, c)) = Source.localDictionaries () in a + b + fromInteger c))

numericProgram :: Sample -> M.MemoristM Int
numericProgram s = do
    bindings <- initialize
    unit <- M.lazy (pure ())
    totals <- replicateM (repeats s) $ do
        a <- sameCaseM bindings unit >>= sumPair
        b <- sameGuardM bindings unit >>= sumPair
        M.PairT c d <- differentM bindings unit
        cv <- M.force c
        dv <- M.force d
        M.PairT e rest <- localDictionariesM bindings unit
        ev <- M.force e
        M.PairT f g <- M.force rest
        fv <- M.force f
        gv <- M.force g
        pure (a + b + cv + fromInteger dv + ev + fv + fromInteger gv)
    pure (sum totals)

numericIdentities :: [Bool]
numericIdentities = M.value (M.eval1 $ do
    bindings <- initialize
    unit <- M.lazy (pure ())
    M.PairT a b <- sameCaseM bindings unit
    M.PairT c d <- sameGuardM bindings unit
    M.PairT e f <- differentM bindings unit
    M.PairT g rest <- localDictionariesM bindings unit
    M.PairT h i <- M.force rest
    M.PairT j _ <- localDictionariesM bindings unit
    pure [M.tname a == M.tname b, M.tname a == M.tname c, M.tname c == M.tname d,
          M.tname a == M.tname e, M.tname e /= M.tname f,
          M.tname g == M.tname h, M.tname g /= M.tname i,
          M.tname g /= M.tname a, M.tname g /= M.tname j])

polymorphicSource :: Sample -> Int
polymorphicSource s = repeats s *
    (let (a, b) = Source.polymorphic ()
         (c, d) = Source.localPolymorphic (undefined :: Int)
     in length a + length b + length c + length d)

polymorphicProgram :: Sample -> M.MemoristM Int
polymorphicProgram s = do
    bindings <- initialize
    unit <- M.lazy (error "polymorphic empty forced its input" :: M.MemoristM ())
    totals <- replicateM (repeats s) $ do
        top <- polymorphicM bindings unit
        localPair <- localPolymorphicM bindings unit
        a <- observeEmptyPair top
        b <- observeEmptyPair localPair
        pure (a + b)
    pure (sum totals)

observeEmptyPair :: M.PairT (L.ListT Int) (L.ListT Bool) -> M.MemoristM Int
observeEmptyPair (M.PairT a b) = do
    left <- M.force a >>= lengthList
    right <- M.force b >>= lengthList
    pure (left + right)

lengthList :: L.ListT a -> M.MemoristM Int
lengthList L.NilT = pure 0
lengthList (L.ConsT _ xs) = (1 +) <$> (M.force xs >>= lengthList)

polymorphicIdentities :: [Bool]
polymorphicIdentities = M.value (M.eval1 $ do
    bindings <- initialize
    unit <- M.lazy (pure ())
    M.PairT a b <- polymorphicM bindings unit
    M.PairT c _ <- polymorphicM bindings unit
    M.PairT d e <- localPolymorphicM bindings unit
    M.PairT f _ <- localPolymorphicM bindings unit
    pure [M.tname a == M.tname b, M.tname a == M.tname c,
          M.tname d == M.tname e, M.tname d /= M.tname a, M.tname d /= M.tname f])
