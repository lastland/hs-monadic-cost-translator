module PayloadComputation where

import qualified Memorist.Memorist as M

project :: M.Th Int -> M.MemoristM Int
project thunk = pure (M.tval thunk)
