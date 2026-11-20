module StandaloneDemandShrinkBypass where

import StandaloneDemandShrink
import qualified Memorist.Memorist as M

direct :: BoxT -> BoxD -> M.MemoristM [BoxD]
direct value demanded = pure (shrinkDemandCompatible value demanded)

forwarded :: BoxT -> BoxD -> M.MemoristM [BoxD]
forwarded value demanded = pure (standalone value demanded)

field :: M.Th Int -> M.ThD Int -> M.MemoristM [M.ThD Int]
field value demanded = pure (shrinkField value demanded)
