module Modify (attempt) where

import Control.Monad.State.Lazy (modify)
import qualified Memorist.Memorist as M

attempt :: M.MemoristM ()
attempt = modify (const M.nzero)
