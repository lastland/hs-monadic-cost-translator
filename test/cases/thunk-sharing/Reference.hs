-- Handwritten from Example.hs. The environment stores the two dictionary
-- specializations exercised here as distinct real suspensions of work 5.
module Generated where

import qualified Memorist.Memorist as M

data Bindings = Bindings (M.Th Int) (M.Th Integer)

initialize :: M.MemoristM Bindings
initialize = do
    intShared <- M.lazy $ do
        five <- M.lazy (pure (5 :: Int))
        work five
    integerShared <- M.lazy $ do
        five <- M.lazy (pure (5 :: Integer))
        work five
    pure (Bindings intShared integerShared)

work :: Num a => M.Th a -> M.MemoristM a
work n = do
    M.tick
    left <- M.force n
    right <- M.force n
    pure (left + right)

workM :: Num a => Bindings -> M.Th a -> M.MemoristM a
workM _ = work

sameM :: Bindings -> M.Th a -> M.MemoristM (M.PairT Int Int)
sameM (Bindings intShared _) _ = M.tick >> pure (M.PairT intShared intShared)

differentM :: Bindings -> M.Th a -> M.MemoristM (M.PairT Int Integer)
differentM (Bindings intShared integerShared) _ =
    M.tick >> pure (M.PairT intShared integerShared)

localM :: Num a => Bindings -> M.Th a -> M.MemoristM (M.PairT a a)
localM bindings x = do
    M.tick
    y <- M.lazy (workM bindings x)
    pure (M.PairT y y)
