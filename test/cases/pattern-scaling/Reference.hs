module Generated (chooseM) where

import qualified Memorist.Memorist as M

-- All equations belong to one function entry. A failed first-column literal
-- must not inspect the second argument.
chooseM :: M.Th Int -> M.Th Int -> M.MemoristM Int
chooseM a b = do
    M.tick
    x <- M.force a
    case x of
        0 -> second 0 100
        1 -> second 1 101
        2 -> second 2 102
        3 -> second 3 103
        4 -> second 4 104
        5 -> second 5 105
        6 -> second 6 106
        7 -> second 7 107
        8 -> second 8 108
        9 -> second 9 109
        _ -> pure 0
  where
    -- Administrative pattern matching, not a source function call.
    second literal answer = do
        y <- M.force b
        pure (if y == literal then answer else 0)
