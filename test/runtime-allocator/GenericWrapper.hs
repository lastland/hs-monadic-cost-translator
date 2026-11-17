module GenericWrapper (attempt) where

import Control.Monad.State.Lazy (MonadState, get, put)
import qualified Memorist.Memorist as M

withState :: MonadState s m => s -> m a -> m a
withState selected action = do
    saved <- get
    put selected
    result <- action
    put saved
    pure result

attempt :: M.MemoristM (M.Th Int)
attempt = withState M.nzero (M.lazy (M.tick >> pure 10))
