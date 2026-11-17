module Generated (asIntM, asIntegerM) where

import qualified Memorist.Memorist as M

-- Keep source-global sharing private to the reasoning model. The enclosing
-- thunk gives every global a negative ancestry, disjoint from caller roots.
globalBindings :: Bindings
globalBindings = M.tval (M.value (M.eval (M.lazy initialize) (M.fromInt (-1))))

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

genericM :: Num a => M.Th a -> M.Th a -> M.MemoristM a
genericM shared argument = do
    M.tick
    n <- M.force shared
    x <- M.force argument
    pure (n + x)

asIntM :: M.Th Int -> M.MemoristM Int
asIntM = asIntWithBindings globalBindings

asIntWithBindings :: Bindings -> M.Th Int -> M.MemoristM Int
asIntWithBindings bindings argument = do
    M.tick
    genericM (intShared bindings) argument

asIntegerM :: M.Th Integer -> M.MemoristM Integer
asIntegerM = asIntegerWithBindings globalBindings

asIntegerWithBindings :: Bindings -> M.Th Integer -> M.MemoristM Integer
asIntegerWithBindings bindings argument = do
    M.tick
    genericM (integerShared bindings) argument
