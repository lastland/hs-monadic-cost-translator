module NewtypeLabelBad where
import NewtypeLabel (Box)
import qualified Memorist.Memorist as M

bad :: Box [Int] -> M.MemoristM Bool
bad = M.reprM
