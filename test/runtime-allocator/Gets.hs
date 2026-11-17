module Gets (attempt) where

import qualified Control.Monad.State.Class as State
import qualified Memorist.Memorist as M

attempt :: M.MemoristM M.ThName
attempt = State.gets id
