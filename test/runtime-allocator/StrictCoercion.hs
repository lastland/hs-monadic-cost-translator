module StrictCoercion (attempt) where

import Control.Monad.Identity (Identity)
import Control.Monad.State.Class (put)
import Data.Coerce (coerce)
import qualified Memorist.Base.Core as Core
import qualified Memorist.Base.Strict as Runtime

attempt :: Runtime.MemoristM ()
attempt = coerce (put (0 :: Int) :: Core.MemoristBaseT Int Identity ())
