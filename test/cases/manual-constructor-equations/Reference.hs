-- Handwritten ordered decisions. Matching/guards do not add ticks; source
-- predicates do. No branch inspects a Cons tail.
module Generated where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

positiveM, negativeM :: M.Th Int -> M.MemoristM Bool
positiveM x = M.tick >> ((> 0) <$> M.force x)
negativeM x = M.tick >> ((< 0) <$> M.force x)

chooseM :: M.Th (L.ListT Int) -> M.MemoristM Int
chooseM xs = do
    M.tick
    value <- M.force xs
    case value of
        L.NilT -> pure 0
        L.ConsT y _ -> do
            yes <- positiveM y
            if yes then M.force y else pure 0

repeatedM :: M.Th (L.ListT Int) -> M.MemoristM Int
repeatedM xs = do
    M.tick
    value <- M.force xs
    case value of
        L.NilT -> pure 0
        L.ConsT y _ -> do
            above <- positiveM y
            if above then M.force y else do
                below <- negativeM y
                if below then negate <$> M.force y else pure 0

wildcardM :: M.Th Bool -> M.Th (L.ListT Int) -> M.MemoristM Int
wildcardM flag xs = do
    M.tick
    yes <- M.force flag
    if yes then pure 7 else inspectHead xs

-- Unlike headValueM, this decision is not a source function invocation.
inspectHead :: M.Th (L.ListT Int) -> M.MemoristM Int
inspectHead xs = do
    value <- M.force xs
    case value of
        L.NilT -> pure 0
        L.ConsT y _ -> M.force y

headValueM :: M.Th (L.ListT Int) -> M.MemoristM Int
headValueM xs = M.tick >> inspectHead xs

variableM :: M.Th Bool -> M.Th (L.ListT Int) -> M.MemoristM Int
variableM flag xs = do
    M.tick
    value <- M.force xs
    case value of
        L.NilT -> pure 0
        L.ConsT y _ -> do
            yes <- M.force flag
            if yes then headValueM xs else M.force y

guardedWildcardM :: M.Th Bool -> M.Th (L.ListT Int) -> M.MemoristM Int
guardedWildcardM flag xs = do
    M.tick
    let pick :: M.Th (L.ListT Int) -> M.MemoristM Int
        pick input = do
            M.tick
            yes <- M.force flag
            if yes then pure 7 else inspectHead input
    pick xs

guardedVariableM :: M.Th Bool -> M.Th (L.ListT Int) -> M.MemoristM Int
guardedVariableM flag xs = do
    M.tick
    let pick :: M.Th (L.ListT Int) -> M.MemoristM Int
        pick input = do
            M.tick
            yes <- M.force flag
            if yes then ignoreList input else inspectHead input
        ignoreList :: M.Th (L.ListT Int) -> M.MemoristM Int
        ignoreList _ = M.tick >> pure 7
    pick xs

fallthroughM :: M.Th Int -> M.Th (L.ListT Int) -> M.MemoristM Int
fallthroughM n xs = do
    M.tick
    value <- M.force xs
    case value of
        L.NilT -> do
            _ <- positiveM n
            M.force n
        L.ConsT y _ -> do
            yes <- positiveM y
            M.force (if yes then y else n)

orderedM :: M.Th Int -> M.MemoristM Int
orderedM x = do
    M.tick
    xs <- M.lazy $ do
        tailThunk <- M.lazy (error "undefined list tail")
        pure (L.ConsT x tailThunk)
    chooseM xs
