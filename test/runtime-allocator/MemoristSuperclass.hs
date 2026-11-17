module MemoristSuperclass (attempt) where

import Control.Monad.State.Class (put)
import qualified Memorist.Memorist as M

reset :: M.Memorist M.ThName m => m ()
reset = put M.nzero

attempt :: M.MemoristM ()
attempt = reset
