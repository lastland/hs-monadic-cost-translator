module ClosedLocalComparison where

import qualified Memorist.Memorist as M

closed :: M.MemoristM Bool
closed =
    let same x y = (M.tick :: M.MemoristM ()) >> pure (x == y)
    in same (1 :: Integer) 2
