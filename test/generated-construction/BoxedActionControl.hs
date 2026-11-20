module BoxedActionControl where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

data Jobs = Jobs { first :: M.MemoristM Bool, spare :: M.MemoristM Bool, flag :: Bool }

runFirst :: Jobs -> Jobs -> M.MemoristM Bool
runFirst (Jobs m _ _) _ = m >>= pure

pass :: a -> a
pass x = x

law :: M.Th Bool -> Q.Property
law t =
    let observed = M.tval t
        jobs = Jobs { flag = observed, spare = pure () >>= \() -> pure (M.tval t), first = pure True }
        unused = Jobs (pure () >>= \() -> pure (M.tval t)) (pure False) False
    in Q.property (flag jobs && M.value (M.eval1 (runFirst (pass jobs) (pass unused))))

pack :: M.MemoristM Bool -> Jobs
pack m = Jobs m (pure False) False

twoCalls :: M.Th Bool -> Q.Property
twoCalls t =
    let used = pack (pure True)
        ignored = pack (pure () >>= \() -> pure (M.tval t))
    in Q.property (M.value (M.eval1 (runFirst used ignored)))

projection :: M.Th Bool -> Q.Property
projection t =
    let jobs = Jobs (pure True) (pure () >>= \() -> pure (M.tval t)) False
    in Q.property (M.value (M.eval1 (spare jobs)))

discard :: M.MemoristM Bool -> M.MemoristM Bool
discard _ = pure True

discardJob :: Jobs -> M.MemoristM Bool
discardJob (Jobs m _ _) = discard (m >>= pure)

discarded :: M.Th Bool -> Q.Property
discarded t =
    let jobs = Jobs (pure () >>= \() -> pure (M.tval t)) (pure False) False
    in Q.property (M.value (M.eval1 (discardJob jobs)))
