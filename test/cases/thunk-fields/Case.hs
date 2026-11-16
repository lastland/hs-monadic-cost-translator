module Case (probes) where

import Control.Monad (replicateM, replicateM_)
import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import qualified Memorist.Base.Thunk as MT
import qualified Memorist.Data.List as L
import ReferenceSupport

probes :: [Probe]
probes = concatMap boxChecks
    [("constructor", Source.constructor, constructorM), ("record", Source.record, recordM)] ++
    [ demanded "tuple/selected" pairProjection pairDemand (\sample -> input sample >>= tupleM)
    , scalarSource "tuple/source" (\sample -> let (a,b) = Source.tuple (number sample) in a+b)
        (\sample -> input sample >>= tupleM >>= repeatPair (sample { repeats = 1 }))
    , scalar "tuple/repeated" (\sample -> input sample >>= tupleM >>= repeatPair sample)
    , demanded "tuple/skipped-error" (const (const ())) (const (const (M.PairD M.Skip M.Skip)))
        (const (unused >>= tupleM))
    , snapshot "tuple/preparation-resources" (const tupleAliases)
    , demanded "list/selected" (\sample -> listProjection (width sample) (leftDemand sample))
        (\sample -> listDemand (width sample) (leftDemand sample)) (\sample -> input sample >>= listM)
    , scalarSource "list/source" (sum . Source.list . number)
        (\sample -> input sample >>= listM >>= sumList)
    , scalar "list/repeated" (\sample -> input sample >>= listM >>= repeatList sample)
    , demanded "list/skipped-heads" (const (listProjection 3 False)) (const (listDemand 3 False))
        (const (unused >>= listM))
    , snapshot "list/preparation-resources" (const listAliases)
    , demanded "cons/selected" (\sample -> listProjection (width sample + 1) (leftDemand sample))
        (\sample -> listDemand (width sample + 1) (leftDemand sample)) consInput
    , scalarSource "cons/source" (\sample -> sum (Source.cons (number sample) (tailValues sample)))
        (\sample -> consInput sample >>= sumList)
    , scalar "cons/repeated" (\sample -> consInput sample >>= repeatList sample)
    , demanded "cons/skipped-tail" (const (listProjection 1 True)) (const (listDemand 1 True)) $ \sample -> do
        x <- input sample
        xs <- M.lazy (error "unused cons tail evaluated" :: M.MemoristM (L.ListT Int))
        consM x xs
    , demanded "cons/skipped-head" (\sample -> listProjection (width sample + 2) False)
        (\sample -> listDemand (width sample + 2) False) $ \sample -> do
        x <- unused
        xs <- M.lazy (costedList (tailValues sample))
        consM x xs
    , snapshot "cons/preparation-resources" (const consAliases)
    , demanded "mixed/selected" boxProjection boxDemand (\sample -> input sample >>= mixedM)
    , scalarSource "mixed/source-left" (\sample -> Source.first (Source.mixed (number sample))) $ \sample -> do
        BoxT a _ <- input sample >>= mixedM
        M.force a
    , demanded "mixed/skipped-computation" (\_ (BoxT a _) -> M.tval a)
        (\_ (BoxT a _) -> BoxD (M.fullDemand a) M.Skip) (\sample -> input sample >>= mixedM)
    , demanded "mixed/skipped-errors" (const (const ())) (const (const (BoxD M.Skip M.Skip)))
        (const (unused >>= mixedM))
    , scalar "mixed/repeated-left" $ \sample -> do
        BoxT a _ <- input sample >>= mixedM
        sum <$> replicateM (repeats sample) (M.force a)
    , failure "mixed/demanded-error" $ \sample -> do
        BoxT _ b <- input sample >>= mixedM
        M.force b
    , failure "bad/demanded-error" (\sample -> M.fromPure1 badM (number sample))
    , snapshot "mixed/preparation-resources" (const (boxAliases mixedM))
    ]
  where
    boxChecks (label, source, make) =
        [ demanded (label ++ "/selected") boxProjection boxDemand (\sample -> input sample >>= make)
        , scalarSource (label ++ "/source")
            (\sample -> let Source.Box a b = source (number sample) in a+b) $ \sample -> do
            BoxT a b <- input sample >>= make
            (+) <$> M.force a <*> M.force b
        , scalar (label ++ "/repeated") $ \sample -> do
            BoxT a b <- input sample >>= make
            repeatPair sample (M.PairT a b)
        , demanded (label ++ "/skipped-error") (const (const ())) (const (const (BoxD M.Skip M.Skip)))
            (const (unused >>= make))
        , snapshot (label ++ "/preparation-resources") (const (boxAliases make))
        ]

input :: Sample -> M.MemoristM (M.Th Int)
input sample = M.lazy (replicateM_ (width sample + 1) M.tick >> pure (number sample))

unused :: M.MemoristM (M.Th Int)
unused = M.lazy (error "unused field evaluated")

boxProjection :: Sample -> BoxT Int -> (Maybe Int, Maybe Int)
boxProjection sample (BoxT a b) = pairProjection sample (M.PairT a b)

boxDemand :: Sample -> BoxT Int -> BoxD Int
boxDemand sample (BoxT a b) =
    let M.PairD da db = pairDemand sample (M.PairT a b)
    in BoxD da db

pairProjection :: Sample -> M.PairT Int Int -> (Maybe Int, Maybe Int)
pairProjection sample (M.PairT a b) =
    (if leftDemand sample then Just (M.tval a) else Nothing,
     if rightDemand sample then Just (M.tval b) else Nothing)

pairDemand :: Sample -> M.PairT Int Int -> M.PairD Int Int
pairDemand sample (M.PairT a b) = M.PairD
    (if leftDemand sample then M.fullDemand a else M.Skip)
    (if rightDemand sample then M.fullDemand b else M.Skip)

repeatPair :: Sample -> M.PairT Int Int -> M.MemoristM Int
repeatPair sample (M.PairT a b) = sum <$> mapM one [1 .. repeats sample]
  where
    one index = do
        first <- M.force (if even (index + variant sample) then a else b)
        second <- M.force (if even (index + variant sample) then b else a)
        pure (first + second)

listProjection :: Int -> Bool -> L.ListT Int -> [Maybe Int]
listProjection depth wanted value
    | depth <= 0 = []
    | otherwise = case value of
        L.NilT -> []
        L.ConsT headThunk tailThunk ->
            (if wanted then Just (M.tval headThunk) else Nothing) :
                listProjection (depth - 1) wanted (M.tval tailThunk)

listDemand :: Int -> Bool -> L.ListT Int -> L.ListD Int
listDemand _ _ L.NilT = L.NilD
listDemand depth wanted (L.ConsT headThunk tailThunk) = L.ConsD
    (if depth > 0 && wanted then M.fullDemand headThunk else M.Skip)
    (if depth > 1 then M.Need (listDemand (depth - 1) wanted (M.tval tailThunk)) else M.Skip)

repeatList :: Sample -> L.ListT Int -> M.MemoristM Int
repeatList sample value = sum <$> replicateM (repeats sample) (sumList value)

sumList :: L.ListT Int -> M.MemoristM Int
sumList L.NilT = pure 0
sumList (L.ConsT headThunk tailThunk) = do
    headValue <- M.force headThunk
    tailValue <- M.force tailThunk
    rest <- sumList tailValue
    pure (headValue + rest)

tailValues :: Sample -> [Int]
tailValues sample = take (width sample) (numbers sample ++ repeat (other sample))

costedList :: [Int] -> M.MemoristM (L.ListT Int)
costedList values = do
    M.tick
    case values of
        [] -> pure L.NilT
        x:xs -> do
            headThunk <- M.lazy (M.tick >> pure x)
            tailThunk <- M.lazy (costedList xs)
            pure (L.ConsT headThunk tailThunk)

consInput :: Sample -> M.MemoristM (L.ListT Int)
consInput sample = do
    x <- input sample
    xs <- M.lazy (costedList (tailValues sample))
    consM x xs

boxAliases :: (M.Th Int -> M.MemoristM (BoxT Int)) -> (Bool, Bool, Bool, Bool, Bool, Integer)
boxAliases make =
    let (result, next) = M.runM (do
            x <- unused
            value <- make x
            pure (M.tname x, value)) M.nzero
        (inputName, BoxT a b) = M.value result
    in (M.tname a == inputName, M.tname b == inputName, M.tname a == M.tname b,
        next == M.nnext inputName, next == M.nnext (M.nnext inputName), MT.toInteger (M.costAnnot result))

tupleAliases :: (Bool, Bool, Integer)
tupleAliases =
    let result = M.eval1 $ do
            x <- unused
            value <- tupleM x
            pure (M.tname x, value)
        (inputName, M.PairT a b) = M.value result
    in (M.tname a == inputName, M.tname b == inputName, MT.toInteger (M.costAnnot result))

listAliases :: (Bool, Bool, Integer)
listAliases =
    let result = M.eval1 $ do
            x <- unused
            value <- listM x
            pure (M.tname x, value)
        (inputName, L.ConsT a tailThunk) = M.value result
        L.ConsT b _ = M.tval tailThunk
    in (M.tname a == inputName, M.tname b == inputName, MT.toInteger (M.costAnnot result))

consAliases :: (Bool, Bool, Integer)
consAliases =
    let result = M.eval1 $ do
            x <- unused
            xs <- M.lazy (error "alias observation forced its tail")
            value <- consM x xs
            pure (M.tname x, M.tname xs, value)
        (headName, tailName, L.ConsT a b) = M.value result
    in (M.tname a == headName, M.tname b == tailName, MT.toInteger (M.costAnnot result))
