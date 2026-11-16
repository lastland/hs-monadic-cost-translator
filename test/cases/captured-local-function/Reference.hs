module Generated where

import qualified Memorist.Memorist as M

doubleM :: Num a => M.Th a -> M.MemoristM a
doubleM x = do
    M.tick
    a <- M.force x
    b <- M.force x
    pure (a + b)

capturedM :: Num a => M.Th a -> M.MemoristM a
capturedM x = do
    M.tick
    y <- M.lazy (doubleM x)
    let add z = do
            M.tick
            a <- M.force y
            b <- M.force z
            pure (a + b)
    a <- add x
    b <- add x
    pure (a + b)
