module Generated where

import qualified Memorist.Memorist as M

mutualM :: M.Th Int -> M.MemoristM Int
mutualM x = do
    M.tick
    let evenN n = do
            M.tick
            value <- M.force n
            if value == (0 :: Int) then M.force x else do
                predecessor <- M.lazy (pure (value - 1))
                oddN predecessor
        oddN n = do
            M.tick
            value <- M.force n
            if value == 0 then pure 0 else do
                predecessor <- M.lazy (pure (value - 1))
                evenN predecessor
    four <- M.lazy (pure 4)
    evenN four
