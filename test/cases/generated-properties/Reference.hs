module Generated (bumpM, mappedM, addM, foldedM, streamM) where

import qualified Memorist.Memorist as M
import Memorist.Data.List (ListT(..))

bumpM :: M.Th Int -> M.MemoristM Int
bumpM x = do
    M.tick
    a <- M.force x
    pure (a + 1)

mappedM :: M.Th (ListT Int) -> M.MemoristM (ListT Int)
mappedM xs = M.tick >> mapList bumpM xs

-- Handwritten map equations: one entry per spine cell (including []). Each
-- callback and recursive tail remains suspended until its field is demanded.
mapList :: (M.Th a -> M.MemoristM b) -> M.Th (ListT a) -> M.MemoristM (ListT b)
mapList f xs = do
    M.tick
    list <- M.force xs
    case list of
        NilT -> pure NilT
        ConsT x rest -> do
            y <- M.lazy (f x)
            ys <- M.lazy (mapList f rest)
            pure (ConsT y ys)

addM :: M.Th Int -> M.Th Int -> M.MemoristM Int
addM x y = do
    M.tick
    a <- M.force x
    b <- M.force y
    pure (a + b)

foldedM :: M.Th (ListT Int) -> M.MemoristM Int
foldedM xs = do
    M.tick
    zero <- M.lazy (pure 0)
    foldList addM zero xs

foldList :: (M.Th a -> M.Th b -> M.MemoristM b) -> M.Th b -> M.Th (ListT a) -> M.MemoristM b
foldList f z xs = do
    M.tick
    list <- M.force xs
    case list of
        NilT -> M.force z
        ConsT x rest -> do
            tailResult <- M.lazy (foldList f z rest)
            f x tailResult

streamM :: M.Th Int -> M.MemoristM (ListT Int)
streamM n = do
    M.tick
    rest <- M.lazy $ do
        next <- M.lazy $ do
            a <- M.force n
            pure (a + 1)
        streamM next
    pure (ConsT n rest)
