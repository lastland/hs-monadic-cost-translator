module Case (probes) where

import Control.Monad (replicateM, replicateM_, (>=>))
import Data.List (elemIndex)
import Generated
import qualified Memorist.Base.Thunk as MT
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport
import qualified ThunkArguments as Source

type Forward = M.Th Int -> M.MemoristM Int
type PairFunction = M.Th Int -> M.MemoristM (M.PairT Int Int)
type MapFunction = M.Th Int -> M.Th (L.ListT Int)
    -> M.MemoristM (L.ListT (M.PairT Int Int))

probes :: [Probe]
probes = concatMap forwarding
    [("ignore", Source.ignore, ignoreM), ("plain", Source.plain, plainM),
     ("parenthesized", Source.parenthesized, parenthesizedM),
     ("annotated", Source.annotated, annotatedM), ("computed", Source.computed, computedM)] ++
    concatMap boxes
    [("constructor", Source.constructor, constructorM), ("record", Source.record, recordM)] ++
    concatMap pairs [("tuple", Source.tuple, tupleM), ("local", Source.local, localM)] ++
    concatMap captures [("partial", Source.partial, partialM), ("composed", Source.composed, composedM)] ++
    [ scalarSource "keep/value" (Source.keep . number) (input >=> keepM)
    , scalarSource "pair/value" (\s -> uncurry (+) (Source.pair (number s) (other s))) $ \s ->
        M.fromPure2 pairM (number s) (other s) >>= sumPair
    , scalarSource "list/value" (sum . Source.list . number) $ \s -> input s >>= listM >>= sumList
    , scalar "list/repeated" $ \s -> do
        value <- input s >>= listM
        sum <$> replicateM (repeats s) (sumList value)
    , demanded "list/unused-fields" (const (const ())) (const (const (L.ConsD M.Skip M.Skip)))
        (const (unused >>= listM))
    , snapshot "list/aliases-and-caller-allocation" (const listResources)
    , scalarSource "cons/value" (\s -> sum (Source.cons (number s) (values s))) $ \s -> do
        x <- input s
        xs <- M.lazy (M.reprM (values s))
        consM x xs >>= sumList
    , scalarSource "cons/unused-tail" (\s -> case Source.cons (number s) undefined of
        [] -> error "source cons returned an empty list"
        y : _ -> y) $ \s -> do
        x <- input s
        xs <- M.lazy (error "unused cons tail")
        value <- consM x xs
        case value of
            L.NilT -> error "cons returned an empty list"
            L.ConsT y _ -> M.force y
    , snapshot "cons/aliases-and-caller-allocation" (const consResources)
    , scalarSource "mixed/unused-computation" (Source.first . Source.mixed . number) $ \s -> do
        BoxT x _ <- input s >>= mixedM
        M.force x
    , demanded "mixed/unused-errors" (const (const ())) (const (const (BoxD M.Skip M.Skip)))
        (const (unused >>= mixedM))
    , snapshot "mixed/aliases-and-caller-allocation" (const (boxResources mixedM))
    , failure "mixed/demanded-computation" $ \s -> do
        BoxT _ y <- input s >>= mixedM
        M.force y
    , failure "bad/demanded" (M.fromPure1 badM . number)
    ]

forwarding :: (String, Int -> Int, Forward) -> [Probe]
forwarding (label, source, call) =
    [ scalarSource (label ++ "/value") (source . number) (input >=> call)
    , scalarSource (label ++ "/ignored-bottom") (const (source undefined)) (const (unused >>= call))
    , scalar (label ++ "/repeated") $ \s -> do
        x <- input s
        sum <$> replicateM (repeats s) (call x)
    , snapshot (label ++ "/caller-allocation") (const (resources (unused >>= call)))
    ]

boxes :: (String, Int -> Source.Box Int, M.Th Int -> M.MemoristM (BoxT Int)) -> [Probe]
boxes (label, source, make) =
    [ demanded (label ++ "/selected") boxProjection boxDemand (input >=> make)
    , scalarSource (label ++ "/value") (\s -> let Source.Box a b = source (number s) in a + b) $ \s -> do
        BoxT a b <- input s >>= make
        (+) <$> M.force a <*> M.force b
    , demanded (label ++ "/ignored-bottom") (const (const ()))
        (const (const (BoxD M.Skip M.Skip))) (const (unused >>= make))
    , snapshot (label ++ "/aliases-and-caller-allocation") (const (boxResources make))
    ]

pairs :: (String, Int -> (Int, Int), PairFunction) -> [Probe]
pairs (label, source, make) =
    [ demanded (label ++ "/selected") pairProjection pairDemand (input >=> make)
    , scalarSource (label ++ "/value") (uncurry (+) . source . number) $ \s -> input s >>= make >>= sumPair
    , scalar (label ++ "/repeated") $ \s -> do
        value <- input s >>= make
        sum <$> replicateM (repeats s) (sumPair value)
    , demanded (label ++ "/ignored-bottom") (const (const ()))
        (const (const (M.PairD M.Skip M.Skip))) (const (unused >>= make))
    , snapshot (label ++ "/aliases-and-caller-allocation") (const (pairResources make))
    ]

captures :: (String, Int -> [Int] -> [(Int, Int)], MapFunction) -> [Probe]
captures (label, source, make) =
    [ scalarSource (label ++ "/value") (\s -> sum [a + b | (a, b) <- source (number s) (values s)]) $ \s -> do
        x <- input s
        xs <- M.lazy (M.reprM (values s))
        make x xs >>= sumPairs True True
    , scalarSource (label ++ "/ignored-capture") (sum . map snd . source undefined . values) $ \s -> do
        x <- unused
        xs <- M.lazy (M.reprM (values s))
        make x xs >>= sumPairs False True
    , scalarSource (label ++ "/ignored-supplied")
        (\s -> sum (map fst (source (number s) (map (const undefined) (values s))))) $ \s -> do
        x <- input s
        xs <- M.lazy (M.reprM (map (const undefined) (values s) :: [Int]))
        make x xs >>= sumPairs True False
    , snapshot (label ++ "/aliases-and-caller-allocation") (captureResources make)
    ]

input :: Sample -> M.MemoristM (M.Th Int)
input s = M.lazy (replicateM_ (width s + 1) M.tick >> pure (number s))

unused :: M.MemoristM (M.Th Int)
unused = M.lazy (error "unused argument or field")

values :: Sample -> [Int]
values s = other s : take (width s) (numbers s ++ repeat (number s))

sumPair :: M.PairT Int Int -> M.MemoristM Int
sumPair (M.PairT a b) = (+) <$> M.force a <*> M.force b

sumList :: L.ListT Int -> M.MemoristM Int
sumList L.NilT = pure 0
sumList (L.ConsT x xs) = (+) <$> M.force x <*> (M.force xs >>= sumList)

sumPairs :: Bool -> Bool -> L.ListT (M.PairT Int Int) -> M.MemoristM Int
sumPairs _ _ L.NilT = pure 0
sumPairs left right (L.ConsT pair rest) = do
    M.PairT a b <- M.force pair
    x <- if left then M.force a else pure 0
    y <- if right then M.force b else pure 0
    tailSum <- M.force rest >>= sumPairs left right
    pure (x + y + tailSum)

pairProjection :: Sample -> M.PairT Int Int -> (Maybe Int, Maybe Int)
pairProjection s (M.PairT a b) =
    (if leftDemand s then Just (M.tval a) else Nothing,
     if rightDemand s then Just (M.tval b) else Nothing)

pairDemand :: Sample -> M.PairT Int Int -> M.PairD Int Int
pairDemand s (M.PairT a b) = M.PairD
    (if leftDemand s then M.fullDemand a else M.Skip)
    (if rightDemand s then M.fullDemand b else M.Skip)

boxProjection :: Sample -> BoxT Int -> (Maybe Int, Maybe Int)
boxProjection s (BoxT a b) = pairProjection s (M.PairT a b)

boxDemand :: Sample -> BoxT Int -> BoxD Int
boxDemand s (BoxT a b) =
    let M.PairD da db = pairDemand s (M.PairT a b)
    in BoxD da db

-- Normalize only the caller's fresh-name progression, not runtime ancestry.
resources :: M.MemoristM a -> (a, Maybe Int, Integer)
resources program =
    let (result, next) = M.runM program M.nzero
    in (M.value result, elemIndex next (take 32 (iterate M.nnext M.nzero)),
        MT.toInteger (M.costAnnot result))

boxResources :: (M.Th Int -> M.MemoristM (BoxT Int)) -> ([Bool], Maybe Int, Integer)
boxResources make = resources $ do
    x <- unused
    BoxT a b <- make x
    pure [M.tname a == M.tname x, M.tname b == M.tname x, M.tname a == M.tname b]

pairResources :: PairFunction -> ([Bool], Maybe Int, Integer)
pairResources make = resources $ do
    x <- unused
    M.PairT a b <- make x
    pure [M.tname a == M.tname x, M.tname b == M.tname x, M.tname a == M.tname b]

listResources :: ([Bool], Maybe Int, Integer)
listResources = resources $ do
    x <- unused
    result <- listM x
    case result of
        L.NilT -> pure []
        L.ConsT a rest -> case M.tval rest of
            L.NilT -> pure [False]
            L.ConsT b _ -> pure [M.tname a == M.tname x, M.tname b == M.tname x]

consResources :: ([Bool], Maybe Int, Integer)
consResources = resources $ do
    x <- unused
    xs <- M.lazy (error "unused cons tail")
    result <- consM x xs
    case result of
        L.NilT -> pure []
        L.ConsT a b -> pure [M.tname a == M.tname x, M.tname b == M.tname xs]

captureResources :: MapFunction -> Sample -> ([(Bool, Bool, Bool)], Maybe Int, Integer)
captureResources make s =
    let ((name, result), next, ticks) = resources $ do
            x <- unused
            xs <- M.lazy (aliasedList x (width s + 1))
            mapped <- make x xs
            pure (M.tname x, mapped)
        aliases L.NilT = []
        aliases (L.ConsT pair rest) =
            let M.PairT a b = M.tval pair
            in (M.tname a == name, M.tname b == name, M.tname a == M.tname b) : aliases (M.tval rest)
    in (aliases result, next, ticks)

aliasedList :: M.Th Int -> Int -> M.MemoristM (L.ListT Int)
aliasedList _ 0 = pure L.NilT
aliasedList x n = L.ConsT x <$> M.lazy (aliasedList x (n - 1))
