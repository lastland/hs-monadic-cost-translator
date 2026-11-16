-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M

thirdM :: M.Th a -> M.Th b -> M.Th c -> M.MemoristM c
thirdM _ _ z = do
    M.tick
    M.force z

observedM :: M.Th Int -> M.MemoristM Int
observedM input = do
    M.tick
    first <- M.lazy (pure (1 :: Int))
    second <- M.lazy (pure (2 :: Int))
    thirdM first second input
