module ModifyStrict (attempt) where

import Control.Monad.State.Strict (modify')
import qualified Memorist.Memorist as M

attempt :: M.MemoristM ()
attempt = modify' (const M.nzero)
