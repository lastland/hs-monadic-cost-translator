module Generated (viewsM) where

import qualified Memorist.Memorist as M

-- The source has one views entry, two same calls, and two shared work calls:
-- full = 5, roots only = 3, skip all = 1. A paid count adds one only when a
-- leaf is demanded; a paid seed adds none because same ignores its argument.
-- Four fully demanded cached calls cost 5; four fresh invocations cost 20.
-- Both uses instantiate shared at (a, Int). Allocate one pair root, not one
-- pair per same call; same's two result roots differ but alias both leaves.
viewsM :: Fractional a => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (M.PairT a Int) (M.PairT a Int))
viewsM seed count = do
    M.tick
    shared <- M.lazy $ do
        left <- M.lazy $ do
            M.tick
            n <- M.force count
            pure (if n < 0 then 6 else 10)
        right <- M.lazy $ do
            M.tick
            n <- M.force count
            pure (if n == 0 then 22 else 14)
        pure (M.PairT left right)
    firstArgument <- M.lazy ((/ 2) <$> M.force seed)
    first <- M.lazy (sameM firstArgument shared)
    secondArgument <- M.lazy ((/ 2) <$> M.force seed)
    second <- M.lazy (sameM secondArgument shared)
    pure (M.PairT first second)

sameM :: M.Th a -> M.Th (M.PairT a Int) -> M.MemoristM (M.PairT a Int)
sameM _ pair = M.tick >> M.force pair
