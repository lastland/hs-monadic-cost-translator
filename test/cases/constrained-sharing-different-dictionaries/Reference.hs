module Generated (Bindings, initialize, asIntM, asIntegerM) where

import qualified Memorist.Memorist as M

data Bindings = Bindings
    { intShared :: M.Th Int
    , integerShared :: M.Th Integer
    }

initialize :: M.MemoristM Bindings
initialize = do
    int <- M.lazy sharedM
    integer <- M.lazy sharedM
    pure (Bindings int integer)

sharedM :: (Eq a, Num a) => M.MemoristM a
sharedM = M.lazy (pure 18446744073709551616) >>= expensiveM

expensiveM :: (Eq a, Num a) => M.Th a -> M.MemoristM a
expensiveM argument = do
    M.tick
    n <- M.force argument
    if n == 0 then pure (n + 1) else do
        zero <- M.lazy (pure (n - n))
        result <- expensiveM zero
        pure (result + 1)

asIntM :: Bindings -> M.Th Int -> M.MemoristM Int
asIntM bindings argument = do
    M.tick
    shared <- M.force (intShared bindings)
    x <- M.force argument
    pure (shared + x)

asIntegerM :: Bindings -> M.Th Integer -> M.MemoristM Integer
asIntegerM bindings argument = do
    M.tick
    shared <- M.force (integerShared bindings)
    x <- M.force argument
    pure (shared + x)
