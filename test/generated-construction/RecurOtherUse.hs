module RecurOtherUse where
import RecurNat (Chain)
import qualified Memorist.Memorist as M

bad :: Chain [Int] -> M.MemoristM (Chain [Int])
bad = M.reprM
