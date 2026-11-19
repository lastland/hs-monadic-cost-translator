module ContextReprUse where

import ContextRepr
import qualified Memorist.Memorist as M

plain :: Box Int -> M.MemoristM (Box Int)
plain = M.reprM

nested :: Nested (Nested (Box Int)) -> M.MemoristM (Nested (Nested (Box Int)))
nested = M.reprM

mixed :: Tagged (Nested Int) Bool -> M.MemoristM (Tagged (Nested Int) Bool)
mixed = M.reprM
