module Generated (HigherT(..), makeM, viewsM) where

import qualified Memorist.Memorist as M

data HigherT f a = HigherT (M.Th Int) (M.Th (M.MaybeT (f a)))

makeM :: M.Th Int -> M.MemoristM (HigherT f a)
makeM input = do
    M.tick
    empty <- M.lazy (pure M.NothingT)
    pure (HigherT input empty)

-- Only the absent optional field needs allocating inside the shared computation.
-- Model its tag by (), independent of f, and retain each allocation's metadata.
viewsM :: M.Th Int -> M.MemoristM (M.PairT (HigherT f Int) (HigherT f Bool))
viewsM input = do
    M.tick
    root <- M.lazy $ do
        M.tick
        M.lazy (pure ())
    pure (M.PairT (view root) (view root))
  where
    view (M.Th root fields charge) = M.Th root (HigherT input (absent fields)) charge
    absent (M.Th field () charge) = M.Th field M.NothingT charge
