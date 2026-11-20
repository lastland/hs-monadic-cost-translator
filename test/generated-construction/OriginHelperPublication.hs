module Generated where

import qualified Example as CallerDatatype
import qualified OriginHelper as Source
import qualified Memorist.Memorist as M

keepM :: M.Th Int -> M.MemoristM Int
keepM x = M.tick >> M.force x

partialM :: M.Th a -> M.MemoristM a
partialM xs = M.tick >> M.force xs

ordinaryRoot :: [Int] -> [Int]
ordinaryRoot xs = map Source.keep xs

structural :: CallerDatatype.Box -> ()
structural _ = ()
