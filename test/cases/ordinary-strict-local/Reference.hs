module Generated (viewsM) where
import qualified Memorist.Memorist as M

-- One source views entry and one demanded work entry. No keeper-entry tick.
-- Forcing a thunk records usage; seq separately consumes its scalar WHNF.
viewsM :: Num t => M.Th t -> M.MemoristM ()
viewsM input = do
    M.tick
    keeper <- M.lazy $ do
        M.tick
        n <- M.force input
        pure (n + n)
    value <- M.force keeper
    value `seq` pure ()
