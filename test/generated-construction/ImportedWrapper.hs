module ImportedWrapper where

import HiddenConstruction ()
import qualified Memorist.Memorist as M

ordinary :: M.MemoristM Int
ordinary = pure 17
