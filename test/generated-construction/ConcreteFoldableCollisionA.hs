module ConcreteFoldableCollisionA where

import qualified Memorist.Memorist as M

{-# LINE 20 "SharedFoldable.hs" #-}
class FoldA t where
    stepA :: M.Th (t a) -> M.MemoristM ()
    inheritedA :: M.Th (t a) -> M.MemoristM ()
    inheritedA xs = stepA xs

instance FoldA M.MaybeT where
    stepA _ = pure ()

selectedA :: M.Th (M.MaybeT a) -> M.MemoristM ()
selectedA = inheritedA

-- A second declaration in this input also has overlapping logical locations.
{-# LINE 20 "SharedFoldable.hs" #-}
class FoldC t where
    stepC :: M.Th (t a) -> M.MemoristM ()
    inheritedC :: M.Th (t a) -> M.MemoristM ()
    inheritedC xs = stepC xs

instance FoldC M.MaybeT where
    stepC _ = pure ()

selectedC :: M.Th (M.MaybeT a) -> M.MemoristM ()
selectedC = inheritedC
