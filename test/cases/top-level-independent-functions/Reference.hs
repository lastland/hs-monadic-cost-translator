module Generated (workM, forwardM, plainM) where

import qualified Memorist.Memorist as M

workM :: M.Th Int -> M.MemoristM Int
workM argument = do
    M.tick
    value <- M.force argument
    pure (value + value)

forwardM :: M.Th Int -> M.MemoristM Int
forwardM argument = do
    M.tick
    doubled <- workM argument
    pure (doubled + 1)

-- Plain pays one entry tick. Each independently demanded field pays two:
-- forward and work. Demanding the whole pair therefore costs five.
plainM :: M.Th Int -> M.MemoristM (M.PairT Int Int)
plainM argument = do
    M.tick
    first <- M.lazy (forwardM argument)
    second <- M.lazy (forwardM argument)
    pure (M.PairT first second)
