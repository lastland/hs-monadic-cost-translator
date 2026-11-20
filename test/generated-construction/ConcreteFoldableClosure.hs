module ConcreteFoldableClosure where

import qualified Memorist.Memorist as M
import qualified Memorist.Data.List as L
import qualified Memorist.Example.List as ML

class FoldableT t where
    foldrT :: M.Th (M.Th a -> M.Th b -> M.MemoristM b)
        -> M.Th b -> M.Th (t a) -> M.MemoristM b
    foldlT :: M.Th (M.Th b -> M.Th a -> M.MemoristM b)
        -> M.Th b -> M.Th (t a) -> M.MemoristM b
    foldlT _ z _ = M.force z
    inherited :: M.Th (M.Th a -> M.Th b -> M.MemoristM b)
        -> M.Th b -> M.Th (t a) -> M.MemoristM b
    inherited f z xs = foldrT f z xs

instance FoldableT M.MaybeT where
    foldrT tf z txs = do
        xs <- M.force txs
        case xs of
            M.NothingT -> M.force z
            M.JustT x -> do
                f <- M.force tf
                f x z

instance FoldableT (M.PairT c) where
    foldrT tf z txs = do
        xs <- M.force txs
        case xs of
            M.PairT _ x -> do
                f <- M.force tf
                f x z

instance FoldableT L.ListT where
    foldrT = ML.foldrM
    foldlT = ML.foldlM

maybeInherited :: M.Th (M.Th a -> M.Th b -> M.MemoristM b)
    -> M.Th b -> M.Th (M.MaybeT a) -> M.MemoristM b
maybeInherited = inherited

pairInherited :: M.Th (M.Th a -> M.Th b -> M.MemoristM b)
    -> M.Th b -> M.Th (M.PairT c a) -> M.MemoristM b
pairInherited = inherited

maybeDirect :: M.Th (M.Th a -> M.Th b -> M.MemoristM b)
    -> M.Th b -> M.Th (M.MaybeT a) -> M.MemoristM b
maybeDirect = foldrT

listRight :: M.Th (M.Th a -> M.Th b -> M.MemoristM b)
    -> M.Th b -> M.Th (L.ListT a) -> M.MemoristM b
listRight = foldrT

listLeft :: M.Th (M.Th b -> M.Th a -> M.MemoristM b)
    -> M.Th b -> M.Th (L.ListT a) -> M.MemoristM b
listLeft = foldlT
