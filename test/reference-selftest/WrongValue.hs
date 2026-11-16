module Generated where
import qualified Memorist.Memorist as M
doubleM :: M.Th Int -> M.MemoristM Int
doubleM x = do M.tick; value <- M.force x; pure (value + value + 1)
ignoreM :: M.Th Int -> M.MemoristM Int
ignoreM _ = do M.tick; pure 42
aliasesM :: M.Th Int -> M.MemoristM (M.PairT Int Int)
aliasesM x = do M.tick; pure (M.PairT x x)
