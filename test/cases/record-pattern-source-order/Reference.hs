-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M

data R = R (M.Th Int) (M.Th Int)

fM :: M.Th R -> M.MemoristM Int
fM record = do
    M.tick
    value <- M.force record
    case value of
        R a b -> do
            -- The source tests b before a, not declaration-field order.
            second <- M.force b
            if second == 0 then do
                first <- M.force a
                if first == 0 then pure 1 else pure 2
            else pure 2

observedM :: M.Th Int -> M.MemoristM Int
observedM _ = do
    M.tick
    record <- M.lazy $ do
        a <- M.lazy (pure undefined)
        b <- M.lazy (pure 1)
        pure (R a b)
    fM record
