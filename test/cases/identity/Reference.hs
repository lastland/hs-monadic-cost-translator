-- Handwritten from Example.f; forcing the argument preserves its sharing.
module Generated (fM) where

import qualified Memorist.Memorist as M

fM :: M.Th a -> M.MemoristM a
fM x = M.tick >> M.force x
