{-# LANGUAGE ScopedTypeVariables #-}
module Generated (fM) where

import qualified Memorist.Memorist as M

-- This unwraps only a pure action, preserving identity's values and costs.
fM :: forall a. M.Th a -> M.MemoristM a
fM arg = M.value (M.eval1 (pure (M.tick >> M.force arg) :: M.MemoristM (M.MemoristM a)))
