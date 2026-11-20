module Generated where

import qualified Memorist.Data.List as L
import qualified Memorist.Example.List as ML
import qualified Memorist.Memorist as M

guardedM, prefixIfM :: M.Th Int -> M.Th Int -> M.MemoristM Int
guardedM x y = do
    M.tick
    a <- M.force x
    b <- M.force y
    pure (if a <= b then a - b else 1)
prefixIfM = guardedM

threeWayM :: M.Th Int -> M.Th Int -> M.MemoristM Int
threeWayM x y = do
    M.tick
    a <- M.force x
    b <- M.force y
    pure (fromEnum (compare a b))

shadowedM :: M.Th Bool -> M.Th Bool -> M.MemoristM Int
shadowedM useGuard flag = do
    M.tick
    first <- M.force useGuard
    let decide yes = do
            enabled <- M.force flag
            pure (if enabled then yes else 0)
    if first then decide 1 else decide 2

knownM :: M.Th Int -> M.MemoristM Int
knownM x = M.tick >> M.force x

mappedM :: M.Th Int -> M.Th (L.ListT Int) -> M.MemoristM (L.ListT Bool)
mappedM lower xs = do
    M.tick
    callback <- M.lazy (pure atLeastLower)
    ML.mapMM callback xs
  where
    atLeastLower element = do
        bound <- M.force lower
        value <- M.force element
        pure (bound <= value)

andIfM, orGuardM :: M.Th Bool -> M.Th Bool -> M.MemoristM Int
andIfM x y = do
    M.tick
    left <- M.force x
    if left then fromEnum <$> M.force y else pure 0
orGuardM x y = do
    M.tick
    left <- M.force x
    if left then pure 1 else fromEnum <$> M.force y
