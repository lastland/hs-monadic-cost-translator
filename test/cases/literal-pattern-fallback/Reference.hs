module Generated where

import qualified Memorist.Memorist as M

chooseM :: M.Th Int -> M.MemoristM Int
chooseM n = do
    M.tick
    value <- M.force n
    if value == 0 then error "zero literal equation" else pure value

literalM :: M.Th Int -> M.MemoristM Int
literalM x = M.tick >> chooseM x
