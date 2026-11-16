-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M

data Pair a b = Pair (M.Th a) (M.Th b)
data Plain a = Plain (M.Th a)

pickPairM :: M.Th (Pair a b) -> M.MemoristM a
pickPairM pair = do
    M.tick
    value <- M.force pair
    case value of
        Pair first _ -> M.force first

pickPlainM :: M.Th (Plain a) -> M.MemoristM a
pickPlainM plain = do
    M.tick
    value <- M.force plain
    case value of
        Plain field -> M.force field

observedM :: M.Th Int -> M.MemoristM Int
observedM input = do
    M.tick
    pair <- M.lazy $ do
        ignored <- M.lazy (pure (undefined :: Int))
        pure (Pair input ignored)
    plain <- M.lazy (pure (Plain input))
    left <- pickPairM pair
    right <- pickPlainM plain
    pure (left + right)
