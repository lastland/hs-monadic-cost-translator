module Generated (viewsM, distinctM) where

import qualified Memorist.Memorist as M

-- Independent cost model: one outer entry, one gate entry per complete
-- dictionary pair, and one work entry for each demanded numeric field.
-- In particular, selecting only the Int field does not identify the roots
-- for (Int, Int) and (Int, Integer).
viewsM :: Num t => M.Th t -> M.Th Int -> M.MemoristM (M.PairT (M.PairT (M.PairT Int Int) (M.PairT Int Int)) (M.PairT (M.PairT Int Integer) (M.PairT Int Integer)))
viewsM _ count = do
    M.tick
    intInt <- M.lazy (pair count)
    intInteger <- M.lazy (pair count)
    first <- M.lazy (pure (M.PairT intInt intInt))
    second <- M.lazy (pure (M.PairT intInteger intInteger))
    pure (M.PairT first second)

pair :: (Num a, Num b) => M.Th Int -> M.MemoristM (M.PairT a b)
pair count = do
    M.tick
    left <- M.lazy $ do
        M.tick
        n <- M.force count
        pure (if n < 0 then error "left failure" else 10)
    right <- M.lazy $ do
        M.tick
        n <- M.force count
        pure (if n == 0 then error "right failure" else 14)
    pure (M.PairT left right)

distinctM :: Num t => M.Th t -> M.MemoristM (M.PairT (M.PairT Int Int) (M.PairT Int Int))
distinctM _ = do
    M.tick
    first <- M.lazy separate
    second <- M.lazy separate
    pure (M.PairT first second)
  where
    separate = do
        M.tick
        left <- M.lazy (M.tick >> pure 10)
        right <- M.lazy (M.tick >> pure 14)
        pure (M.PairT left right)
