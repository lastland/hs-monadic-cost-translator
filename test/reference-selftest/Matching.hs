module Generated where
import qualified Memorist.Memorist as M
doubleM :: M.Th Int -> M.MemoristM Int
doubleM input = do
    M.tick
    n <- M.force input
    pure (2 * n)
ignoreM :: M.Th Int -> M.MemoristM Int
ignoreM _ = M.tick >> pure 42
aliasesM :: M.Th Int -> M.MemoristM (M.PairT Int Int)
aliasesM input = M.tick >> pure (M.PairT input input)
