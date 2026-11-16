module Generated where

import qualified Memorist.Memorist as M

keepM :: M.Th a -> M.MemoristM a
keepM x = do
    M.tick
    unused <- M.lazy (error "unused")
    if True || (undefined :: Bool) then M.force x else M.force unused
