module StrictRecordUpdate (attempt) where

import Control.Monad.State.Class (put)
import qualified Memorist.Base.Strict as Runtime

attempt :: Runtime.MemoristM ()
attempt = (pure () :: Runtime.MemoristM ()) { Runtime.runMT = put (0 :: Int) }
