module Generated where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

guardedM :: M.Th Int -> M.MemoristM Int
guardedM x = do
    M.tick
    ys <- M.lazy $ do
        rest <- M.lazy (error "undefined list tail")
        pure (L.ConsT x rest)
    list <- M.force ys
    case list of
        L.ConsT h _ -> do
            z <- M.lazy $ do
                a <- M.force h
                b <- M.force h
                pure (a + b)
            value <- M.force z
            if value > 0 && True then M.force z else error "failed case guard"
        L.NilT -> error "failed pattern guard"
