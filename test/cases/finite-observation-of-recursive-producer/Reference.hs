module Generated where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

streamM :: Num a => M.Th a -> M.MemoristM (L.ListT a)
streamM n = do
    M.tick
    successor <- M.lazy $ do
        value <- M.force n
        pure (value + 1)
    rest <- M.lazy (streamM successor)
    pure (L.ConsT n rest)

secondM :: M.Th (L.ListT a) -> M.MemoristM a
secondM xs = do
    M.tick
    list <- M.force xs
    case list of
        L.NilT -> error "second: empty list"
        L.ConsT _ rest -> do
            tailList <- M.force rest
            case tailList of
                L.NilT -> error "second: singleton"
                L.ConsT x _ -> M.force x

observeM :: Num a => M.Th a -> M.MemoristM a
observeM n = do
    M.tick
    xs <- M.lazy (streamM n)
    secondM xs
