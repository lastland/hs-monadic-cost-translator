module Generated (viewsM) where

import qualified Memorist.Memorist as M

-- The source's unused value allocates nothing. Only views pays an entry tick;
-- a paid count contributes once when demanded. Cached calls cost one entry,
-- while r fresh calls cost r entries.
viewsM :: M.Th Int -> M.MemoristM Int
viewsM count = M.tick >> M.force count
