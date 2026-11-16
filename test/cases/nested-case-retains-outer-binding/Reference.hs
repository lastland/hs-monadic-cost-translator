module Generated where

import qualified Memorist.Memorist as M

nestedM :: Num a => M.Th a -> M.MemoristM a
nestedM x = do
    M.tick
    let y = x
    z <- M.lazy $ do
        a <- M.force x
        b <- M.force x
        pure (a + b)
    a <- M.force y
    b <- M.force z
    pure (a + b)
