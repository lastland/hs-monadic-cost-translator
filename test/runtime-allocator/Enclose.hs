module Enclose (attempt) where

import Control.Monad (void)
import qualified Memorist.Base.ANT as Runtime

attempt :: Runtime.MemoristM ()
attempt = void (Runtime.enclose (pure ()))
