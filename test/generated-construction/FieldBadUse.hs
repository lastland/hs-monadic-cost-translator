module FieldBadUse where
import FieldUnused (Box)
import FieldProjection (Project)
import FieldChain (Indirect, Outer)
import ContextRepr (Tagged)
import qualified Memorist.Memorist as M

unused :: Box [Int] -> M.MemoristM (Box [Int])
unused = M.reprM

projected :: Project [Int] -> M.MemoristM Bool
projected = M.reprM

chain :: Outer [Int] -> M.MemoristM (Tagged Int Bool)
chain = M.reprM

indirect :: Indirect [Bool] -> M.MemoristM Bool
indirect = M.reprM
