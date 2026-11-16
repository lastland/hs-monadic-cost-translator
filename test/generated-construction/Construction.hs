{-# LANGUAGE BlockArguments #-}
module Construction where

import qualified Memorist.Memorist as M

ordinary :: M.MemoristM Int
ordinary = do
    thunk <- M.lazy do
        M.tick
        pure 3
    M.force thunk

conversion :: M.Th Int -> Int
conversion = M.tval

-- M.value (M.eval1 action) is only a comment, not a reference.
description :: String
description = "Memorist.Base.Core.eval M.eval1 M.value runM runMMBT runMBT runAnnotT runMT"
