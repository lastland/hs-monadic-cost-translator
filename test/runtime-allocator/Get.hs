module Get (attempt) where

import Control.Monad.State.Lazy (get)
import qualified Memorist.Memorist as M

attempt :: M.MemoristM M.ThName
attempt = get
