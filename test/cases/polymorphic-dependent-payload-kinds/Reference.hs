{-# LANGUAGE DataKinds, PolyKinds #-}
module Generated (PacketT(..), IndexT(..), viewsM) where

import Data.Kind (Type)
import Data.Void (Void, absurd)
import qualified Memorist.Memorist as M

data PacketT (f :: k -> Type) (a :: k) = PacketT (M.Th Int) (M.Th (M.MaybeT (f a)))
data IndexT (a :: Bool) = IndexT (M.Th Int)

mapThunk :: (a -> b) -> M.Th a -> M.Th b
mapThunk f (M.Th name body annotation) = M.Th name (f body) annotation

emptyView :: M.MaybeT Void -> M.MaybeT a
emptyView M.NothingT = M.NothingT
emptyView (M.JustT leaf) = M.JustT (mapThunk absurd leaf)

packetView :: M.Th (M.PairT Int (M.MaybeT Void)) -> M.Th (PacketT f a)
packetView = mapThunk convert
  where
    convert (M.PairT fixed optional) = PacketT fixed (mapThunk emptyView optional)

-- The root is allocated once as an ordinary pair. Both kind-indexed views retain
-- its identity and the field identities. No type-class constraint is needed.
-- Source-entry costs: skip/root/optional = 1; first/full/cached = 2; paid = 3.
-- r fresh calls cost 2*r, with disjoint root/fixed/optional names.
viewsM :: M.Th t -> M.Th Int -> M.MemoristM (M.PairT (PacketT M.MaybeT Int) (PacketT IndexT 'True))
viewsM _ count = do
    M.tick
    root <- M.lazy $ do
        fixed <- M.lazy $ do
            M.tick
            n <- M.force count
            pure (n + n)
        optional <- M.lazy (pure M.NothingT)
        pure (M.PairT fixed optional)
    pure (M.PairT (packetView root) (packetView root))
