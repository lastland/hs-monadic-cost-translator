module Unqualified where

import Memorist.Base.Core (Annotated, eval1)
import Memorist.Base.ANT (MemoristM)

evaluate :: MemoristM Int -> Annotated Int
evaluate = eval1
