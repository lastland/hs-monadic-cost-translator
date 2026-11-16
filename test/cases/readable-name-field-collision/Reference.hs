-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M

pickM :: M.Th Int -> M.Th (M.PairT (M.PairT Int Int) Int) -> M.MemoristM Int
pickM input pair = do
    M.tick
    outer <- M.force pair
    case outer of
        M.PairT inner _ -> do
            fields <- M.force inner
            case fields of
                M.PairT first _ -> do
                    left <- M.force input
                    right <- M.force first
                    pure (left + right)

observedM :: M.Th Int -> M.MemoristM Int
observedM input = do
    M.tick
    pair <- M.lazy $ do
        inner <- M.lazy $ do
            first <- M.lazy (pure 3)
            second <- M.lazy (pure 0)
            pure (M.PairT first second)
        lastField <- M.lazy (pure 0)
        pure (M.PairT inner lastField)
    pickM input pair
