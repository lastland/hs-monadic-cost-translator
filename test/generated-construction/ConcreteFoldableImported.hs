module ConcreteFoldableImported where

import ConcreteFoldableHidden
import ConcreteFoldableClosure
import qualified Memorist.Memorist as M

selected :: M.Th (M.Th a -> M.Th b -> M.MemoristM b)
    -> M.Th b -> M.Th (Hidden a) -> M.MemoristM b
selected = inherited
