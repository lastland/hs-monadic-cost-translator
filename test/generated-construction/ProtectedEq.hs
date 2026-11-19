module ProtectedEq where

import qualified Memorist.Memorist as M

same :: M.Th Int -> M.Th Int -> M.MemoristM Bool
same left right = pure (left == right)

-- Desugaring can discard this binding; the source occurrence still needs evidence.
unused :: M.Th Int -> M.MemoristM Int
unused thunk = let hidden = thunk == thunk in pure 3
