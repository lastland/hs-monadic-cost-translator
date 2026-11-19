module ConcreteConstructionControl where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

ordinary :: M.MemoristM Int
ordinary = do
    thunk <- M.lazy (M.tick >> pure 3)
    M.force thunk

returned :: M.MemoristM Int
returned = return 17

binding :: M.MemoristM Int
binding = returned >>= \number -> M.tick >> pure number

applicative :: M.MemoristM Int
applicative = (+) <$> returned <*> binding

scalar :: M.MemoristM Int
scalar = M.reprM (17 :: Int)

optional :: M.MemoristM (M.MaybeT Int)
optional = M.reprM (Just (17 :: Int))

pair :: M.MemoristM (M.PairT Int Bool)
pair = M.reprM ((17 :: Int), True)

list :: M.MemoristM (L.ListT (M.MaybeT Int))
list = M.reprM [Just (17 :: Int), Nothing]
