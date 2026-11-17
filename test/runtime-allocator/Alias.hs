module Alias (attempt) where

import qualified Control.Monad.State.Class as State
import qualified Memorist.Memorist as M

type Computation = M.MemoristM

reset :: State.MonadState M.ThName m => m ()
reset = State.put M.nzero

attempt :: Computation ()
attempt = reset
