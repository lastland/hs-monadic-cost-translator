module UnusedHelper where

import qualified Memorist.Memorist as M

ordinary :: Int
ordinary = let _unused = M.eval1 (pure () :: M.MemoristM ()) in 1
