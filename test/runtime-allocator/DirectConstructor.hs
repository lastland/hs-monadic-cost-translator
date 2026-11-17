module DirectConstructor (attempt) where

import Control.Monad.State.Class (put)
import qualified Memorist.Base.ANT as Runtime
import qualified Memorist.Base.Thunk as Thunk

attempt :: Runtime.MemoristM ()
attempt = Runtime.MemoristT (put Thunk.nzero)
