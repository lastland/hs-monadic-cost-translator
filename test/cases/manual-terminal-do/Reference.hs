-- Handwritten conditionals keep one entry tick and demand only the selected
-- branch. Source lets still allocate their own suspension.
module Generated where

import qualified Memorist.Memorist as M

chooseM :: M.Th Bool -> M.Th a -> M.Th a -> M.MemoristM a
chooseM condition yes no = do
    M.tick
    selected <- M.force condition
    M.force (if selected then yes else no)

workM :: M.Th Int -> M.MemoristM Int
workM x = do
    M.tick
    value <- M.force x
    pure (value + value)

badM :: M.Th a -> M.MemoristM a
badM _ = M.tick >> error "unselected branch evaluated"

computedM, localM :: M.Th Bool -> M.Th Int -> M.Th Int -> M.MemoristM Int
computedM condition x y = do
    M.tick
    yes <- M.lazy (workM x)
    no <- M.lazy (workM y)
    chooseM condition yes no
localM condition x y = do
    M.tick
    saved <- M.lazy (workM x)
    no <- M.lazy (workM y)
    chooseM condition saved no

ignoredFailureM :: M.Th Bool -> M.Th Int -> M.MemoristM Int
ignoredFailureM condition x = do
    M.tick
    yes <- M.lazy (workM x)
    no <- M.lazy (badM x)
    chooseM condition yes no

annotatedM :: M.Th Bool -> M.Th a -> M.Th a -> M.MemoristM a
annotatedM condition x y = do
    M.tick
    selected <- M.force condition
    if selected then M.force x else do
        saved <- M.lazy (M.force y)
        M.force saved
