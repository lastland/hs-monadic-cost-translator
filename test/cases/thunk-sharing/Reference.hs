-- Handwritten from Example.hs. The environment stores the two dictionary
-- specializations exercised here as distinct real suspensions of work 5.
module Generated (workM, sameM, differentM, localM) where

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

-- The source-sharing model has one closed context. Suspending initialization
-- keeps both dictionary caches under negative ancestry, outside caller names.
globalBindings :: Bindings
globalBindings = M.tval (M.value (M.eval (M.lazy initialize) (M.fromInt (-1))))

work :: Num a => M.Th a -> M.MemoristM a
work n = do
    M.tick
    left <- M.force n
    right <- M.force n
    pure (left + right)

workM :: Num a => M.Th a -> M.MemoristM a
workM = work

sameM :: M.Th a -> M.MemoristM (M.PairT Int Int)
sameM = sameWith globalBindings

sameWith :: Bindings -> M.Th a -> M.MemoristM (M.PairT Int Int)
sameWith (Bindings intShared _) _ = M.tick >> pure (M.PairT intShared intShared)

differentM :: M.Th a -> M.MemoristM (M.PairT Int Integer)
differentM = differentWith globalBindings

differentWith :: Bindings -> M.Th a -> M.MemoristM (M.PairT Int Integer)
differentWith (Bindings intShared integerShared) _ =
    M.tick >> pure (M.PairT intShared integerShared)

localM :: Num a => M.Th a -> M.MemoristM (M.PairT a a)
localM x = do
    M.tick
    y <- M.lazy (workM x)
    pure (M.PairT y y)
