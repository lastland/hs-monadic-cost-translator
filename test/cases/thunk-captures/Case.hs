module Case (probes) where

import Control.Monad (replicateM, replicateM_)
import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import qualified Memorist.Base.Thunk as MT
import qualified Memorist.Data.List as L
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "pair/source" (\sample -> let (a,b) = Source.pair (number sample) (other sample) in a+b) $ \sample -> do
        M.PairT a b <- M.fromPure2 pairM (number sample) (other sample)
        (+) <$> M.force a <*> M.force b
    , scalarSource "keep/source" (Source.keep . number)
        (\sample -> M.fromPure1 keepM (number sample))
    ] ++ concatMap check [("partial", Source.partial, partialM), ("composed", Source.composed, composedM)]
  where
    check (label, source, make) =
        [ demanded (label ++ "/selected")
            (\sample -> projectList (width sample) (leftDemand sample) (rightDemand sample))
            (\sample -> demandList (width sample) (leftDemand sample) (rightDemand sample))
            (normalInput make)
        , scalarSource (label ++ "/source")
            (\sample -> sum [a+b | (a,b) <- source (number sample) (values sample)])
            (\sample -> normalInput make sample >>= sumPairs True True)
        , scalar (label ++ "/repeated") $ \sample -> do
            result <- normalInput make sample
            sum <$> replicateM (repeats sample)
                (sumPairs (leftDemand sample) (rightDemand sample) result)
        , scalarSource (label ++ "/skipped-capture")
            (\sample -> sum (map snd (source (error "unused source capture") (values sample)))) $ \sample -> do
            x <- M.lazy (error "unused captured argument evaluated")
            xs <- M.lazy (costedList False (values sample))
            make x xs >>= sumPairs False True
        , scalarSource (label ++ "/skipped-supplied")
            (\sample -> sum (map fst (source (number sample) (map (const (error "unused source element")) (values sample))))) $ \sample -> do
            x <- captured sample
            xs <- M.lazy (costedList True (values sample))
            make x xs >>= sumPairs True False
        , demanded (label ++ "/skipped-errors")
            (\sample -> projectList (width sample + 1) False False)
            (\sample -> demandList (width sample + 1) False False) $ \sample -> do
            x <- M.lazy (error "unused captured argument evaluated")
            xs <- M.lazy (costedList True (values sample))
            make x xs
        , failure (label ++ "/captured-error") $ \sample -> do
            x <- M.lazy (error "demanded captured argument")
            xs <- M.lazy (costedList False [number sample])
            mapped <- make x xs
            case mapped of
                L.NilT -> pure 0
                L.ConsT pair _ -> do
                    M.PairT a _ <- M.force pair
                    M.force a
        , failure (label ++ "/supplied-error") $ \sample -> do
            x <- captured sample
            xs <- M.lazy (costedList True [other sample])
            mapped <- make x xs
            case mapped of
                L.NilT -> pure 0
                L.ConsT pair _ -> do
                    M.PairT _ b <- M.force pair
                    M.force b
        , snapshot (label ++ "/preparation-resources") (aliasResources make)
        ]

type Make = M.Th Int -> M.Th (L.ListT Int) -> M.MemoristM (L.ListT (M.PairT Int Int))

values :: Sample -> [Int]
values sample = number sample : take (width sample) (numbers sample ++ repeat (other sample))

captured :: Sample -> M.MemoristM (M.Th Int)
captured sample = M.lazy (replicateM_ (width sample + 1) M.tick >> pure (number sample))

normalInput :: Make -> Sample -> M.MemoristM (L.ListT (M.PairT Int Int))
normalInput make sample = do
    x <- captured sample
    xs <- M.lazy $ do
        M.tick
        tailThunk <- M.lazy (costedList False (tail (values sample)))
        pure (L.ConsT x tailThunk)
    make x xs

costedList :: Bool -> [Int] -> M.MemoristM (L.ListT Int)
costedList poison items = do
    M.tick
    case items of
        [] -> pure L.NilT
        x:xs -> do
            headThunk <- M.lazy (if poison then error "unused supplied argument evaluated" else M.tick >> pure x)
            tailThunk <- M.lazy (costedList poison xs)
            pure (L.ConsT headThunk tailThunk)

projectList :: Int -> Bool -> Bool -> L.ListT (M.PairT Int Int) -> [(Maybe Int, Maybe Int)]
projectList depth left right value
    | depth <= 0 = []
    | otherwise = case value of
        L.NilT -> []
        L.ConsT headThunk tailThunk ->
            let M.PairT a b = M.tval headThunk
            in (if left then Just (M.tval a) else Nothing,
                if right then Just (M.tval b) else Nothing) :
                projectList (depth - 1) left right (M.tval tailThunk)

demandList :: Int -> Bool -> Bool -> L.ListT (M.PairT Int Int) -> L.ListD (M.PairD Int Int)
demandList _ _ _ L.NilT = L.NilD
demandList depth left right (L.ConsT headThunk tailThunk) = L.ConsD
    (if depth > 0 then
        let M.PairT a b = M.tval headThunk
        in M.Need (M.PairD (if left then M.fullDemand a else M.Skip)
                          (if right then M.fullDemand b else M.Skip))
     else M.Skip)
    (if depth > 1 then M.Need (demandList (depth - 1) left right (M.tval tailThunk)) else M.Skip)

sumPairs :: Bool -> Bool -> L.ListT (M.PairT Int Int) -> M.MemoristM Int
sumPairs _ _ L.NilT = pure 0
sumPairs left right (L.ConsT headThunk tailThunk) = do
    M.PairT a b <- M.force headThunk
    av <- if left then M.force a else pure 0
    bv <- if right then M.force b else pure 0
    rest <- M.force tailThunk >>= sumPairs left right
    pure (av + bv + rest)

aliasResources :: Make -> Sample -> ([(Bool, Bool, Bool)], Integer)
aliasResources make sample =
    let result = M.eval1 $ do
            x <- M.lazy (error "alias observation forced captured argument")
            xs <- M.lazy (aliasedList x (width sample + 1))
            mapped <- make x xs
            pure (M.tname x, mapped)
        (inputName, mapped) = M.value result
        relations L.NilT = []
        relations (L.ConsT headThunk tailThunk) =
            let M.PairT a b = M.tval headThunk
            in (M.tname a == inputName, M.tname b == inputName, M.tname a == M.tname b) :
                relations (M.tval tailThunk)
    in (relations mapped, MT.toInteger (M.costAnnot result))

aliasedList :: M.Th Int -> Int -> M.MemoristM (L.ListT Int)
aliasedList _ 0 = pure L.NilT
aliasedList x count = do
    tailThunk <- M.lazy (aliasedList x (count - 1))
    pure (L.ConsT x tailThunk)
