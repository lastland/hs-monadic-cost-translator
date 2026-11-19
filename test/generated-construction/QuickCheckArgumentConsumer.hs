module QuickCheckArgumentConsumer where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

apply :: (M.Th Bool -> Q.Gen Bool) -> M.Th Bool -> M.MemoristM Bool
apply k x =
    k x `seq` pure True
