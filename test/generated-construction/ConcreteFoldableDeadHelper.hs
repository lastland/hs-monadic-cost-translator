module ConcreteFoldableDeadHelper where

import qualified Memorist.Memorist as M

class Dead t where
    run :: M.Th (t a) -> M.MemoristM ()
    inherited :: M.Th (t a) -> M.MemoristM ()
    inherited xs =
        let dead () = run xs
        in pure ()
