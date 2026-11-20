module ConcreteFoldableClosure where

import qualified Memorist.Memorist as M

class FoldableT t where
    foldrT :: M.Th (M.Th a -> M.Th b -> M.MemoristM b)
        -> M.Th b -> M.Th (t a) -> M.MemoristM b
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

maybeInherited :: M.Th (M.Th a -> M.Th b -> M.MemoristM b)
    -> M.Th b -> M.Th (M.MaybeT a) -> M.MemoristM b
maybeInherited = inherited

pairInherited :: M.Th (M.Th a -> M.Th b -> M.MemoristM b)
    -> M.Th b -> M.Th (M.PairT c a) -> M.MemoristM b
pairInherited = inherited

maybeDirect :: M.Th (M.Th a -> M.Th b -> M.MemoristM b)
    -> M.Th b -> M.Th (M.MaybeT a) -> M.MemoristM b
maybeDirect = foldrT
