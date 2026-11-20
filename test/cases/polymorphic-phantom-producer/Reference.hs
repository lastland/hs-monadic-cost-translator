{-# LANGUAGE KindSignatures #-}
module Generated (ParcelT, Body(..), viewsM, inferredM) where

import Data.Kind (Type)
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

-- The phantom parameter carries no data. An independent model can use the
-- same monomorphic thunk for both components, without any metadata operation.
data Body a = MissingT (M.Th a) | PackedT (M.Th a) (M.Th Int) (M.Th Int)
type ParcelT (f :: Type -> Type) a = Body a

viewsM :: M.Th a -> M.Th Bool -> M.Th Int -> M.Th Int
    -> M.MemoristM (M.PairT (ParcelT L.ListT a) (ParcelT M.MaybeT a))
viewsM tag present left right = do
    M.tick
    pair tag present left right

type Views a = M.PairT (Body a) (Body a)

pair :: M.Th a -> M.Th Bool -> M.Th Int -> M.Th Int -> M.MemoristM (Views a)
pair tag present left right = do
    root <- M.lazy $ do
        M.tick
        selected <- M.force present
        if selected then PackedT tag <$> M.lazy (work left) <*> M.lazy (work right)
                    else pure (MissingT tag)
    pure (M.PairT root root)
  where
    work input = do
        M.tick
        n <- M.force input
        pure (n + n)

inferredM :: M.Th Int -> M.Th Bool -> M.MemoristM (M.PairT (Views Int) (Views Bool))
inferredM n b = do
    M.tick
    let go tag = do
            M.tick
            present <- M.lazy (pure True)
            pair tag present n n
    M.PairT <$> M.lazy (go n) <*> M.lazy (go b)
