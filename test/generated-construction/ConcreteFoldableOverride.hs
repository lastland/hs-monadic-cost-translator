module ConcreteFoldableOverride where

import ConcreteFoldableClosure
import qualified Memorist.Memorist as M

data Evil a = Evil

-- A checked sibling and a safe class default do not certify this override.
instance FoldableT Evil where
    foldrT _ z _ = M.force z
    inherited _ z _ = pure (M.tval z)

selected :: M.Th (M.Th a -> M.Th b -> M.MemoristM b)
    -> M.Th b -> M.Th (Evil a) -> M.MemoristM b
selected = inherited
