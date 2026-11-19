module RecurFlipBad where
import RecurFlip (Checked)
import qualified Memorist.Memorist as M

-- The root compares Bool; only the swapped state's sibling compares [Int].
bad :: Checked Bool [Int] -> M.MemoristM (Checked Bool [Int])
bad = M.reprM
