module Generated where

import Prelude hiding (mapM)
import qualified Memorist.Memorist as M

mapM :: Num a => M.Th a -> M.MemoristM a
mapM x = do
    M.tick
    a <- M.force x
    b <- M.force x
    pure (a + b)

wrappedM :: Num a => M.Th a -> M.MemoristM a
wrappedM x = M.tick >> mapM x
