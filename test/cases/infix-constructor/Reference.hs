-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M

data Pair a = Pair (M.Th a) (M.Th a)

firstM :: M.Th (Pair a) -> M.MemoristM a
firstM pair = do
    M.tick
    value <- M.force pair
    case value of
        Pair first _ -> M.force first

pairedM :: M.Th Int -> M.MemoristM Int
pairedM input = do
    M.tick
    pair <- M.lazy $ do
        ignored <- M.lazy (pure undefined)
        pure (Pair input ignored)
    firstM pair
