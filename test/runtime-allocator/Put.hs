module Put (attempt) where

import Control.Monad.State.Strict (put)
import qualified Memorist.Memorist as M

attempt :: M.MemoristM ()
attempt = put M.nzero
