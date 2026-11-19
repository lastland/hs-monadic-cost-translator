module Generated (viewsM) where

import qualified Memorist.Memorist as M

-- One views entry and one work entry give full cost 2; roots and skip cost 1.
-- Both copies share one Int dictionary instance, root, and payload per call.
-- Cached repeated demands cost 2; r fresh calls cost 2*r. A shared paid count
-- adds one only when at least one payload is demanded, even across fresh calls.
viewsM :: M.Th Int -> M.MemoristM (M.PairT (M.MaybeT Int) (M.MaybeT Int))
viewsM count = do
    M.tick
    shared <- M.lazy $ do
        payload <- M.lazy $ do
            M.tick
            n <- M.force count
            pure (if n == 0 then 6 else 10)
        pure (M.JustT payload)
    pure (M.PairT shared shared)
