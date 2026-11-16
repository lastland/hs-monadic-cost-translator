module Generated (matchFirstM, guardFirstM, demandedM, unusedM, unmatchedM) where

import qualified Memorist.Memorist as M
import Memorist.Data.List (ListT(..))

matchFirstM :: M.Th Int -> M.Th (ListT Int) -> M.MemoristM Int
matchFirstM n xs = do
    M.tick
    list <- M.force xs
    case list of
        ConsT x _ -> do
            k <- M.force n
            if k <= 0 then pure 0 else M.force x
        NilT -> error "matchFirst: non-exhaustive equations"

guardFirstM :: M.Th Int -> M.Th (ListT Int) -> M.MemoristM Int
guardFirstM n xs = do
    M.tick
    k <- M.force n
    if k <= 0 then pure 0 else do
        list <- M.force xs
        case list of
            ConsT x _ -> M.force x
            NilT -> error "guardFirst: non-exhaustive equations"

demandedM :: M.Th Int -> M.MemoristM Int
demandedM x = do
    M.tick
    n <- M.lazy (pure 0)
    xs <- M.lazy (error "undefined list")
    a <- matchFirstM n xs
    b <- M.force x
    pure (a + b)

unusedM :: M.Th Int -> M.MemoristM Int
unusedM x = do
    M.tick
    n <- M.lazy (pure 0)
    xs <- M.lazy (error "undefined list")
    a <- guardFirstM n xs
    b <- M.force x
    pure (a + b)

unmatchedM :: M.Th Int -> M.MemoristM Int
unmatchedM x = do
    M.tick
    case NilT :: ListT Int of
        ConsT y _ -> do
            a <- M.force x
            b <- M.force y
            pure (a + b)
        NilT -> error "unmatched: non-exhaustive case"
