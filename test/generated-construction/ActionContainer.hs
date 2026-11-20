module ActionContainer where

import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

data Job = Job { task :: M.MemoristM Bool }

runJobs :: Maybe [Job] -> M.MemoristM Bool
runJobs (Just (j : _)) = do
    flag <- task j
    pure flag
runJobs _ = pure False

handed :: M.Th Bool -> Q.Property
handed t =
    let observed = M.tval t
        jobs = Just [Job (pure () >>= \() -> pure observed), Job (pure True)]
    in Q.property (M.value (M.eval1 (runJobs jobs)))

pack :: M.MemoristM Bool -> Job
pack action = Job action

packed :: M.Th Bool -> Q.Property
packed t =
    let jobs = Just [pack (pure () >>= \() -> pure (M.tval t))]
        runner = runJobs
    in Q.property (M.value (M.eval1 (runner $ jobs)))

runAfter :: Job -> M.MemoristM Bool
runAfter = \(Job action) -> pure () >>= \() -> action

after :: M.Th Bool -> Q.Property
after t =
    let box = Job {task = pure () >>= \() -> pure (M.tval t)}
    in Q.property (M.value (M.eval1 (runAfter box)))
