module RecordUpdate (attempt) where

import Control.Monad.State.Class (put)
import qualified Memorist.Memorist as M

attempt :: M.MemoristM ()
attempt = (pure () :: M.MemoristM ()) { M.runMT = put M.nzero }
