module Generated where

import qualified Memorist.Memorist as M

workM :: Num a => M.Th a -> M.MemoristM a
workM x = do
    M.tick
    a <- M.force x
    b <- M.force x
    pure (a + b)

addM :: Num a => M.Th a -> M.Th a -> M.MemoristM a
addM x y = do
    M.tick
    a <- M.force x
    b <- M.force y
    pure (a + b)

sharedM :: Num a => M.Th a -> M.MemoristM a
sharedM x = do
    M.tick
    y <- M.lazy (workM x)
    addM y y
