module Generated (viewsM, skippedM, failedM, dependentM) where
import qualified Memorist.Memorist as M

-- All three strict paths demand one work call; the forward-reference tuple
-- adds no entry tick and its unused Bool field stays suspended.
viewsM, failedM, dependentM :: Num t => M.Th t -> M.MemoristM ()
viewsM input = do
    M.tick
    keeper <- M.lazy $ do
        M.tick
        value <- M.force input
        pure (value + value)
    value <- M.force keeper
    value `seq` pure ()
failedM = viewsM
dependentM = viewsM

skippedM :: Num t => M.Th t -> M.MemoristM ()
skippedM _ = M.tick
