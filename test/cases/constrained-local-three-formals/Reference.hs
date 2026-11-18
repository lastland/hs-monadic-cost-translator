module Generated (viewsM, distinctM) where

import qualified Memorist.Memorist as M

-- Handwritten source model, not a translation/allocation algorithm. Each complete
-- dictionary triple owns its root and fields. The gate ignores its argument and
-- costs one entry. Work doubles a literal and costs one entry when demanded.
-- Full: 1 outer + 2 gates + 6 work = 9; common: 1+2+4=7; last: 1+2+2=5.
viewsM :: Num t => M.Th t -> M.Th Int -> M.MemoristM (M.PairT (M.PairT (M.PairT Int (M.PairT Int Int)) (M.PairT Int (M.PairT Int Int))) (M.PairT (M.PairT Int (M.PairT Int Integer)) (M.PairT Int (M.PairT Int Integer))))
viewsM _ count = do
    M.tick
    ints <- M.lazy (triple count)
    integers <- M.lazy (triple count)
    first <- M.lazy (pure (M.PairT ints ints))
    second <- M.lazy (pure (M.PairT integers integers))
    pure (M.PairT first second)

triple :: (Num a, Num b, Num c) => M.Th Int -> M.MemoristM (M.PairT a (M.PairT b c))
triple count = do
    M.tick
    left <- M.lazy $ do
        M.tick
        n <- M.force count
        pure (if n < 0 then error "left failure" else 10)
    middle <- M.lazy (M.tick >> pure 14)
    lastField <- M.lazy $ do
        M.tick
        n <- M.force count
        pure (if n == 0 then error "last failure" else 18)
    tailPair <- M.lazy (pure (M.PairT middle lastField))
    pure (M.PairT left tailPair)

distinctM :: Num t => M.Th t -> M.MemoristM (M.PairT (M.PairT Int (M.PairT Int Int)) (M.PairT Int (M.PairT Int Int)))
distinctM _ = do
    M.tick
    first <- M.lazy separate
    second <- M.lazy separate
    pure (M.PairT first second)
  where
    separate = do
        M.tick
        a <- M.lazy (M.tick >> pure 10)
        b <- M.lazy (M.tick >> pure 14)
        c <- M.lazy (M.tick >> pure 18)
        rest <- M.lazy (pure (M.PairT b c))
        pure (M.PairT a rest)
