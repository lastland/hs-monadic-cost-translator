module StrictConstructor (attempt) where

import Control.Monad.State.Class (put)
import qualified Memorist.Base.Strict as Runtime

attempt :: Runtime.MemoristM ()
attempt = Runtime.MemoristT (put (0 :: Int))
