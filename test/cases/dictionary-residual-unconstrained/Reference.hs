module Generated (workM, viewsM) where

import Data.Void (Void, absurd)
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

workM :: Num a => M.Th a -> M.MemoristM a
workM input = do
    M.tick
    n <- M.force input
    pure (n + n)

mapThunk :: (a -> b) -> M.Th a -> M.Th b
mapThunk f (M.Th name value annotation) = M.Th name (f value) annotation

listView :: L.ListT Void -> L.ListT a
listView L.NilT = L.NilT
listView (L.ConsT head tail) =
    L.ConsT (mapThunk absurd head) (mapThunk listView tail)

view :: M.PairT Int (L.ListT Void) -> M.PairT Int (L.ListT a)
view (M.PairT number list) = M.PairT number (mapThunk listView list)

viewsM :: M.Th Int -> M.MemoristM
    (M.PairT (M.PairT Int (L.ListT Bool)) (M.PairT Int (L.ListT Char)))
viewsM count = do
    M.tick
    root <- M.lazy $ do
        number <- M.lazy $ do
            M.tick
            n <- M.force count
            pure (if n == 0 then 6 else 10)
        list <- M.lazy (pure L.NilT)
        pure (M.PairT number list)
    pure (M.PairT (mapThunk view root) (mapThunk view root))
