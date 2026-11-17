module StrictGet (attempt) where

import Control.Monad.State.Class (get)
import qualified Memorist.Base.Strict as M

attempt :: M.MemoristM Int
attempt = get
