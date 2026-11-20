module Generated where

import qualified Example as Source
import qualified Memorist.Memorist as M

keepM :: M.Th Int -> M.MemoristM Int
keepM x = M.tick >> M.force x

partialM :: M.Th a -> M.MemoristM a
partialM xs = M.tick >> M.force xs

left :: Int -> Int
left = (Source.keep $)

right :: (([Int] -> [Int]) -> [Int]) -> [Int]
right = ($ Source.partial)

first :: Int -> Int
{-# LINE 100 "colliding-origin-section.hs" #-}
first = (Source.keep $)

other :: Int -> Int
{-# LINE 100 "colliding-origin-section.hs" #-}
other = (Source.keep $)
