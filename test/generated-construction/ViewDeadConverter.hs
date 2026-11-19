module ViewDeadConverter where

import StructuralViewControl (preserve)
import qualified Memorist.Memorist as M

hidden :: M.Th Int -> M.Th Int
hidden input = preserve (\value -> let unused = M.value (M.eval1 (pure value :: M.MemoristM Int)) in value) input
