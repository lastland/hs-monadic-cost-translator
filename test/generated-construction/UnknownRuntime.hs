module UnknownRuntime where

import qualified Memorist.Base.Extension as Runtime
import qualified Memorist.Memorist as M

unknown :: M.Th (M.Th b -> M.MemoristM c) -> M.Th (M.Th a -> M.MemoristM b) -> M.MemoristM (M.Th a -> M.MemoristM c)
unknown = Runtime.composeTM
