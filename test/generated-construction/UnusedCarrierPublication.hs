module Generated where

import qualified Memorist.Memorist as M

fM :: M.Th Int -> M.MemoristM Int
fM input = M.tick >> M.force input

unused :: M.Annotated Int
unused = pure 17
