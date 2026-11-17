module Coercion (attempt) where

import Control.Monad.Identity (Identity)
import Control.Monad.State.Class (put)
import Data.Coerce (coerce)
import qualified Memorist.Memorist as M

attempt :: M.MemoristM ()
attempt = coerce (put M.nzero :: M.MemoristBaseT M.ThName Identity ())
