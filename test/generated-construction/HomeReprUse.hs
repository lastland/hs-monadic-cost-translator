module HomeReprUse where

import HomeReprControl (Box(..))
import qualified Memorist.Memorist as M

selected :: M.MemoristM (M.MaybeT Box)
selected = M.reprM (Just (Box 1))
