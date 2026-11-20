module BoxSupportHelperEscape where

import BoxSupportHelpers
import qualified Memorist.Memorist as M

alias = payload

ordinary :: M.Th Int -> M.MemoristM Int
ordinary x = pure (alias x)
