module BoxedAction where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

data Job = Job (M.MemoristM Bool)

runJob :: Job -> M.MemoristM Bool
runJob (Job m) = m >>= pure

handed :: M.Th Int -> Q.Property
handed t =
    let job = Job (pure () >>= \() -> M.tval t `seq` pure True)
    in Q.property (M.value (M.eval1 (runJob job)))
