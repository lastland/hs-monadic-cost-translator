module ConcreteFoldableHomeInstance where

import ConcreteFoldableClosure
import qualified Memorist.Memorist as M

data Home a = Home

-- The class equation and this actual dictionary live in different home modules.
instance FoldableT Home where
    foldrT _ z _ = M.force z

selected :: M.Th (M.Th a -> M.Th b -> M.MemoristM b)
    -> M.Th b -> M.Th (Home a) -> M.MemoristM b
selected = inherited
