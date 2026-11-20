module BoxSupportBypass where

import BoxSupport
import qualified Memorist.Memorist as M

payload :: M.Th Int -> M.MemoristM Int
payload x = pure (M.tval x)

observed :: BoxT -> M.MemoristM Box
observed x = pure (M.abstract x)

demanded :: BoxT -> M.MemoristM BoxD
demanded x = pure (M.fullDemand x)

accounted x d = M.usage x d
