module Generated (identityM) where

import qualified Memorist.Memorist as M

-- Minimal.identity has one function entry and no strict primitive operation.
identityM :: M.Th Int -> M.MemoristM Int
identityM x = M.tick >> M.force x
