module Generated (localM) where

import qualified Memorist.Memorist as M

expensiveM :: (Eq a, Num a) => M.Th a -> M.MemoristM a
expensiveM argument = do
    M.tick
    n <- M.force argument
    if n == 0 then pure (n + 1) else do
        zero <- M.lazy (pure (n - n))
        result <- expensiveM zero
        pure (result + 1)

sharedM :: (Eq a, Num a) => M.MemoristM a
sharedM = M.lazy (pure 18446744073709551616) >>= expensiveM

localM :: M.Th Int -> M.MemoristM Int
localM argument = do
    M.tick
    -- These belong to this invocation, not to a top-level bindings record.
    sharedInt <- M.lazy sharedM :: M.MemoristM (M.Th Int)
    sharedInteger <- M.lazy sharedM :: M.MemoristM (M.Th Integer)
    let one number = do
            M.tick
            n <- M.force sharedInt
            x <- M.force number
            pure (n + x)
        two number = do
            M.tick
            n <- M.force sharedInteger
            x <- M.force number
            pure (n + x)
    intZero <- M.lazy (pure 0)
    first <- one intZero
    if first == 1 then do
        integerZero <- M.lazy (pure 0)
        second <- two integerZero
        if second == 2 then M.force argument else pure 0
    else pure 0
