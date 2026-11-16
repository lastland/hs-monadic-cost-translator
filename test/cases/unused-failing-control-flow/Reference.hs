module Generated where

import qualified Memorist.Memorist as M

keepM :: M.Th a -> M.MemoristM a
keepM x = do
    M.tick
    _unused <- M.lazy $
        if (undefined :: Bool) then M.force x else M.force x
    M.force x
