module Generated (viewsM, inferredM, distinctM) where

import qualified Memorist.Memorist as M

expensiveM :: (Eq a, Num a) => M.Th a -> M.MemoristM a
expensiveM input = do
    M.tick
    n <- M.force input
    if n == 0 then pure (n + 1) else do
        zero <- M.lazy (pure (n - n))
        result <- expensiveM zero
        pure (result + 1)

ignoredM :: Num a => M.Th a -> M.MemoristM Bool
ignoredM _ = M.tick >> pure True

-- Reference-only construction helpers add no source-function entry ticks.
-- On the supported 64-bit baseline, the large literal becomes zero at Int.
-- Its Int/Integer producers return 1/2 with 1/2 ticks, respectively.
literalWork :: (Eq a, Num a) => Integer -> M.MemoristM a
literalWork literal = M.lazy (pure (fromInteger literal)) >>= expensiveM

guardedWork :: (Eq a, Num a) => M.Th Int -> M.MemoristM a
guardedWork input = do
    n <- M.force input
    if n < 0 then error "demanded local failure"
    else literalWork 18446744073709551616

inferredWork :: (Num outer, Eq result, Num result) => M.Th outer -> M.MemoristM result
inferredWork input = do
    keep <- ignoredM input
    if keep then literalWork 18446744073709551616 else literalWork 0

viewsM :: M.Th t -> M.Th Int -> M.MemoristM (M.PairT (M.PairT Int Int) (M.PairT Integer Integer))
viewsM _ count = do
    M.tick
    -- One native allocation for each dictionary, retained by both aliases.
    sharedInt <- M.lazy (guardedWork count :: M.MemoristM Int)
    sharedInteger <- M.lazy (guardedWork count :: M.MemoristM Integer)
    ints <- M.lazy (pure (M.PairT sharedInt sharedInt))
    integers <- M.lazy (pure (M.PairT sharedInteger sharedInteger))
    pure (M.PairT ints integers)

inferredM :: Num a => M.Th a -> M.MemoristM (M.PairT (M.PairT Int Int) (M.PairT Integer Integer))
inferredM input = do
    M.tick
    sharedInt <- M.lazy (inferredWork input :: M.MemoristM Int)
    sharedInteger <- M.lazy (inferredWork input :: M.MemoristM Integer)
    ints <- M.lazy (pure (M.PairT sharedInt sharedInt))
    integers <- M.lazy (pure (M.PairT sharedInteger sharedInteger))
    pure (M.PairT ints integers)

distinctM :: M.Th t -> M.MemoristM (M.PairT (M.PairT Int Int) (M.PairT Integer Integer))
distinctM _ = do
    M.tick
    -- The source bindings stay separate even when their dictionaries agree.
    firstInt <- M.lazy (literalWork 18446744073709551616 :: M.MemoristM Int)
    secondInt <- M.lazy (literalWork 18446744073709551616 :: M.MemoristM Int)
    firstInteger <- M.lazy (literalWork 18446744073709551616 :: M.MemoristM Integer)
    secondInteger <- M.lazy (literalWork 18446744073709551616 :: M.MemoristM Integer)
    ints <- M.lazy (pure (M.PairT firstInt secondInt))
    integers <- M.lazy (pure (M.PairT firstInteger secondInteger))
    pure (M.PairT ints integers)
