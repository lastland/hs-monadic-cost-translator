{-# LANGUAGE TypeFamilies #-}
-- Handwritten from Example.hs. Constructor fields reuse variables and suspend
-- computations; demand accounts for the original suspension only once.
module Generated where

import qualified Memorist.Memorist as M
import qualified Memorist.Data.List as L

data BoxT a = BoxT (M.Th a) (M.Th a)
data BoxD a = BoxD (M.ThD a) (M.ThD a)

instance M.Demand a => M.Demand (BoxT a) where
    type Dem (BoxT a) = BoxD (M.Dem a)
    fullDemand (BoxT a b) = BoxD (M.fullDemand a) (M.fullDemand b)
    usage (BoxT a b) (BoxD da db) = M.union (M.usage a da) (M.usage b db)

constructorM, recordM :: M.Th a -> M.MemoristM (BoxT a)
constructorM x = M.tick >> pure (BoxT x x)
recordM x = M.tick >> pure (BoxT x x)

tupleM :: M.Th a -> M.MemoristM (M.PairT a a)
tupleM x = M.tick >> pure (M.PairT x x)

listM :: M.Th a -> M.MemoristM (L.ListT a)
listM x = do
    M.tick
    tailThunk <- M.lazy $ do
        nil <- M.lazy (pure L.NilT)
        pure (L.ConsT x nil)
    pure (L.ConsT x tailThunk)

consM :: M.Th a -> M.Th (L.ListT a) -> M.MemoristM (L.ListT a)
consM x xs = M.tick >> pure (L.ConsT x xs)

badM :: M.Th a -> M.MemoristM a
badM _ = M.tick >> error "computed field evaluated"

mixedM :: M.Th a -> M.MemoristM (BoxT a)
mixedM x = do
    M.tick
    computed <- M.lazy (badM x)
    pure (BoxT x computed)
