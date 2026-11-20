module ConcreteFoldableOpen where

import ConcreteFoldableClosure
import qualified Memorist.Memorist as M

ordinary :: FoldableT t => M.Th (M.Th a -> M.Th b -> M.MemoristM b)
    -> M.Th b -> M.Th (t a) -> M.MemoristM b
ordinary = foldrT

-- A class default owns one exact dictionary, not another given of that class.
class Other t where
    own :: M.Th (t a) -> M.MemoristM ()
    other :: Other u => M.Th (t a) -> M.Th (u a) -> M.MemoristM ()
    other xs ys = do
        own xs
        own ys

-- A generic helper does not inherit a default's self authority.
relay :: FoldableT t => M.Th (M.Th a -> M.Th b -> M.MemoristM b)
    -> M.Th b -> M.Th (t a) -> M.MemoristM b
relay = inherited
