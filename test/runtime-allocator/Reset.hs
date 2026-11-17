module Reset (attempt) where

import Control.Monad.State.Lazy (get, put)
import qualified Memorist.Memorist as M

attempt :: M.MemoristM (M.Th Int)
attempt = do
    saved <- get
    put M.nzero
    thunk <- M.lazy (M.tick >> pure 10)
    put saved
    pure thunk
