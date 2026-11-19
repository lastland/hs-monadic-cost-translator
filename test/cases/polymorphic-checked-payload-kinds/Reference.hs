{-# LANGUAGE KindSignatures #-}
module Generated (BoxT(..), viewsM) where

import Data.Kind (Type)
import qualified Memorist.Memorist as M

data BoxT (f :: Type -> Type -> Type) = BoxT (M.Th Int) (M.Th (M.MaybeT (f Int Bool)))

-- Source entries: views costs one; work costs one only when fixed is demanded.
-- Skip, root, optional: 1. First/full/repeated cached full: 2.
-- A paid count adds one only on fixed demand. The ignored input is never paid.
-- r fresh calls with one count cost 2*r (or 2*r+1 with a shared paid count).
-- A bottom count is safe for roots/optional, and fails on fixed demand.
-- Both views share the root and its two fields; fresh calls share none of them.
viewsM :: M.Th t -> M.Th Int -> M.MemoristM (M.PairT (BoxT M.PairT) (BoxT M.PairT))
viewsM _ count = do
    M.tick
    root <- M.lazy $ do
        fixed <- M.lazy $ do
            M.tick
            n <- M.force count
            pure (n + n)
        optional <- M.lazy (pure M.NothingT)
        pure (BoxT fixed optional)
    pure (M.PairT root root)
