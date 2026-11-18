module Generated (viewsM, aliasesM) where
import qualified Memorist.Memorist as M

work :: M.Th t -> M.MemoristM (M.PairT t Bool)
work input = do
    M.tick
    unused <- M.lazy (pure undefined)
    pure (M.PairT input unused)

viewsM :: M.Th t -> M.MemoristM ()
viewsM input = do
    M.tick
    keeper <- M.lazy (work input)
    pair <- M.force keeper
    pair `seq` pure ()

aliasesM :: M.Th t -> M.MemoristM (M.PairT (M.PairT t Bool) (M.PairT t Bool))
aliasesM input = do
    M.tick
    keeper <- M.lazy (work input)
    pair <- M.force keeper
    pair `seq` pure (M.PairT keeper keeper)
