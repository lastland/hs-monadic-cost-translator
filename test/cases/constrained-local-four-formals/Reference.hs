module Generated (viewsM, distinctM) where

import qualified Memorist.Memorist as M

-- Independent, handwritten source model: roots are keyed by the complete four
-- dictionaries, although the first three are equal. Doubles of 5/7/9/11 sum to
-- 64 per view. Full cost 1+2+8=11; common first three 1+2+6=9; last 1+2+2=5.
viewsM :: Num t => M.Th t -> M.Th Int -> M.MemoristM (M.PairT (M.PairT (M.PairT Int (M.PairT Int (M.PairT Int Int))) (M.PairT Int (M.PairT Int (M.PairT Int Int)))) (M.PairT (M.PairT Int (M.PairT Int (M.PairT Int Integer))) (M.PairT Int (M.PairT Int (M.PairT Int Integer)))))
viewsM _ count = do
    M.tick
    ints <- M.lazy (quadruple count)
    integers <- M.lazy (quadruple count)
    first <- M.lazy (pure (M.PairT ints ints))
    second <- M.lazy (pure (M.PairT integers integers))
    pure (M.PairT first second)

quadruple :: (Num a, Num b, Num c, Num d) => M.Th Int -> M.MemoristM (M.PairT a (M.PairT b (M.PairT c d)))
quadruple count = do
    M.tick
    left <- M.lazy $ do
        M.tick
        n <- M.force count
        pure (if n < 0 then error "left failure" else 10)
    second <- M.lazy (M.tick >> pure 14)
    third <- M.lazy (M.tick >> pure 18)
    lastField <- M.lazy $ do
        M.tick
        n <- M.force count
        pure (if n == 0 then error "last failure" else 22)
    inner <- M.lazy (pure (M.PairT third lastField))
    rest <- M.lazy (pure (M.PairT second inner))
    pure (M.PairT left rest)

distinctM :: Num t => M.Th t -> M.MemoristM (M.PairT (M.PairT Int (M.PairT Int (M.PairT Int Int))) (M.PairT Int (M.PairT Int (M.PairT Int Int))))
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
        d <- M.lazy (M.tick >> pure 22)
        inner <- M.lazy (pure (M.PairT c d))
        rest <- M.lazy (pure (M.PairT b inner))
        pure (M.PairT a rest)
