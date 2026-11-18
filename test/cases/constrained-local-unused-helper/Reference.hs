module Generated (viewsM) where

import qualified Memorist.Memorist as M

viewsM :: Num t => M.Th t -> M.MemoristM ()
viewsM _ = M.tick
