-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M

data R a = Infix (M.Th a) (M.Th a) | Named (M.Th a)

pickM :: M.Th (R Int) -> M.MemoristM Int
pickM record = do
    M.tick
    value <- M.force record
    case value of
        Infix first _ -> M.force first
        Named field -> do
            left <- M.force field
            right <- M.force field
            pure (left + right)

observedM :: M.Th Int -> M.MemoristM Int
observedM input = do
    M.tick
    infixValue <- M.lazy $ do
        ignored <- M.lazy (pure undefined)
        pure (Infix input ignored)
    namedValue <- M.lazy (pure (Named input))
    left <- pickM infixValue
    right <- pickM namedValue
    pure (left + right)
