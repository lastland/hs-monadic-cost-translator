module ConcreteFoldableHidden where

import ConcreteFoldableClosure
import qualified Memorist.Memorist as M

data Hidden a = Hidden

instance FoldableT Hidden where
    foldrT _ z _ =
        let erased = M.tval z
        in M.force z

observe :: M.Th b -> M.MemoristM b
observe z = pure (M.tval z)

data Helper a = Helper

instance FoldableT Helper where
    foldrT _ z _ = observe z
