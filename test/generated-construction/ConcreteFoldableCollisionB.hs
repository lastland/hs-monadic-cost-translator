module ConcreteFoldableCollisionB where

import qualified Memorist.Memorist as M

{-# LINE 20 "SharedFoldable.hs" #-}
class FoldB t where
    stepB :: M.Th (t a) -> M.MemoristM ()
    inheritedB :: M.Th (t a) -> M.MemoristM ()
    inheritedB xs = stepB xs

instance FoldB M.MaybeT where
    stepB _ = pure ()

selectedB :: M.Th (M.MaybeT a) -> M.MemoristM ()
selectedB = inheritedB
