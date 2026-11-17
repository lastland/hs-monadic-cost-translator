module TransformerWrapper (attempt) where

import Control.Monad.Reader (lift, runReaderT)
import Control.Monad.State.Class (put)
import qualified Memorist.Memorist as M

attempt :: M.MemoristM ()
attempt = runReaderT (lift (put M.nzero)) ()
