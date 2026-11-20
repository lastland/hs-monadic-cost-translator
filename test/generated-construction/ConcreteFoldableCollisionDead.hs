module ConcreteFoldableCollisionDead where

import ConcreteFoldableCollisionA
import ConcreteFoldableCollisionB
import qualified Memorist.Memorist as M

{-# LINE 20 "SharedFoldable.hs" #-}
class FoldD t where
    stepD :: M.Th (t a) -> M.MemoristM ()
    inheritedD :: M.Th (t a) -> M.MemoristM ()
    inheritedD xs =
        let dead () = stepD xs
        in pure ()
