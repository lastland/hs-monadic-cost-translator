{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module ErasedHomeMethod where

import qualified Memorist.Memorist as M
import QuickCheckRoles (Draw, Make(..))

-- GHC removes the redundant equation from Core; the source still selects make.
probe :: () -> M.MemoristM ()
probe _ = pure ()
probe _ = (make :: Draw Int) `seq` pure ()
