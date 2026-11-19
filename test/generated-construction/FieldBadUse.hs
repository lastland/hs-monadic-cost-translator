module FieldBadUse where
import FieldUnused (Box)
import FieldProjection (Project)
import qualified Memorist.Memorist as M

unused :: Box [Int] -> M.MemoristM (Box [Int])
unused = M.reprM

projected :: Project [Int] -> M.MemoristM Bool
projected = M.reprM
