module Generated (asIntM, asIntegerM) where

import qualified Memorist.Memorist as M

-- Keep source-global sharing private to the reasoning model. The enclosing
-- thunk gives every global a negative ancestry, disjoint from caller roots.
globalBindings :: Bindings
globalBindings = M.tval (M.value (M.eval (M.lazy initialize) (M.fromInt (-1))))

data Bindings = Bindings
    { intOuter :: M.Th Int
    , integerOuter :: M.Th Integer
    }

initialize :: M.MemoristM Bindings
initialize = do
    int <- M.lazy outerM
    integer <- M.lazy outerM
    pure (Bindings int integer)

-- Each dictionary-specific outer thunk allocates its own local inner thunk.
outerM :: (Eq a, Num a) => M.MemoristM a
outerM = do
    inner <- M.lazy (do
        five <- M.lazy (pure (5 :: Int))
        workM five)
    n <- M.force inner
    if n > 0 then M.lazy (pure 18446744073709551616) >>= expensiveM
        else pure 0

workM :: M.Th Int -> M.MemoristM Int
workM argument = do
    M.tick
    n <- M.force argument
    pure (n + n)

expensiveM :: (Eq a, Num a) => M.Th a -> M.MemoristM a
expensiveM argument = do
    M.tick
    n <- M.force argument
    if n == 0 then pure (n + 1) else do
        zero <- M.lazy (pure (n - n))
        result <- expensiveM zero
        pure (result + 1)

asIntM :: M.Th Int -> M.MemoristM Int
asIntM = asIntWithBindings globalBindings

asIntWithBindings :: Bindings -> M.Th Int -> M.MemoristM Int
asIntWithBindings bindings argument = do
    M.tick
    shared <- M.force (intOuter bindings)
    x <- M.force argument
    pure (shared + x)

asIntegerM :: M.Th Integer -> M.MemoristM Integer
asIntegerM = asIntegerWithBindings globalBindings

asIntegerWithBindings :: Bindings -> M.Th Integer -> M.MemoristM Integer
asIntegerWithBindings bindings argument = do
    M.tick
    shared <- M.force (integerOuter bindings)
    x <- M.force argument
    pure (shared + x)
