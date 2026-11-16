module Generated (plainM) where

import qualified Memorist.Memorist as M

plainM :: M.Th Int -> M.MemoristM (M.PairT Int Int)
plainM argument = M.tick >> pure (M.PairT argument argument)
