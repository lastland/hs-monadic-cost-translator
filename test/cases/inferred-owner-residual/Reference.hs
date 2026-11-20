module Generated (viewsM) where

import Data.Void (Void, absurd)
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

type Copies k = M.PairT (M.PairT k (L.ListT Bool)) (M.PairT k (L.ListT Char))

mapThunk :: (a -> b) -> M.Th a -> M.Th b
mapThunk f (M.Th n x a) = M.Th n (f x) a

listView :: L.ListT Void -> L.ListT a
listView L.NilT = L.NilT
listView (L.ConsT h t) = L.ConsT (mapThunk absurd h) (mapThunk listView t)

view :: M.PairT k (L.ListT Void) -> M.PairT k (L.ListT a)
view (M.PairT num xs) = M.PairT num (mapThunk listView xs)

copies :: Integral k => M.Th Int -> M.MemoristM (Copies k)
copies count = do
    M.tick -- helper entry; a new root for each invocation
    root <- M.lazy $ do
        num <- M.lazy $ do
            M.tick -- paid entry
            n <- M.force count
            pure (if n == 0 then 3 else 5)
        xs <- M.lazy (pure L.NilT)
        pure (M.PairT num xs)
    M.tick -- tie entry does not demand its key
    pure (M.PairT (mapThunk view root) (mapThunk view root))

viewsM :: Integral a => M.Th a -> M.Th Int -> M.MemoristM (M.PairT (Copies a) (Copies Integer))
viewsM _ count = do
    M.tick
    a <- M.lazy (copies count)
    b <- M.lazy (copies count)
    pure (M.PairT a b)
