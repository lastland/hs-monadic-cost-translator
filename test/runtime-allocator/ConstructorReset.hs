module ConstructorReset (attempt) where

import Control.Monad.State.Lazy (get, put)
import qualified Memorist.Memorist as M

-- This is the original bad helper, injected through the public wrapper rather
-- than a MonadState instance. Both routes must be unavailable to clients.
attempt :: M.MemoristM (M.Th Int)
attempt = do
    saved <- M.MemoristT get
    M.MemoristT (put M.nzero)
    thunk <- M.lazy (M.tick >> pure 10)
    M.MemoristT (put saved)
    pure thunk
