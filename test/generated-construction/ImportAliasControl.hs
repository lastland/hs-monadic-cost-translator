module ImportAliasControl where

import qualified UserNames as M

ordinaryUserOperations :: Int -> Int
ordinaryUserOperations = M.value . M.eval . M.eval1 . M.runM . M.runMMBT . M.runMT . M.runMBT . M.runAnnotT
