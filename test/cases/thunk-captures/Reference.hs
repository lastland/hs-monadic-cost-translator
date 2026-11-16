-- Handwritten from Example.hs. Mapping suspends each application; partial
-- application keeps x itself, while composition suspends the call to keep.
module Generated where

import qualified Memorist.Memorist as M
import qualified Memorist.Data.List as L

pairM :: M.Th a -> M.Th b -> M.MemoristM (M.PairT a b)
pairM a b = M.tick >> pure (M.PairT a b)

keepM :: M.Th a -> M.MemoristM a
keepM x = M.tick >> M.force x

partialM :: M.Th a -> M.Th (L.ListT b) -> M.MemoristM (L.ListT (M.PairT a b))
partialM x xs = do
    M.tick
    function <- M.lazy (pure (pairM x))
    mapReference function xs

composedM :: M.Th a -> M.Th (L.ListT b) -> M.MemoristM (L.ListT (M.PairT a b))
composedM x xs = do
    M.tick
    function <- M.lazy $ pure $ \supplied -> do
        intermediate <- M.lazy (keepM supplied)
        pairM x intermediate
    mapReference function xs

mapReference :: M.Th (M.Th a -> M.MemoristM b) -> M.Th (L.ListT a)
    -> M.MemoristM (L.ListT b)
mapReference function input = do
    M.tick
    value <- M.force input
    case value of
        L.NilT -> pure L.NilT
        L.ConsT headThunk tailThunk -> do
            apply <- M.force function
            mappedHead <- M.lazy (apply headThunk)
            mappedTail <- M.lazy (mapReference function tailThunk)
            pure (L.ConsT mappedHead mappedTail)
