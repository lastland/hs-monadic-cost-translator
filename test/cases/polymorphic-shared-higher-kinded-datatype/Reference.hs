{-# LANGUAGE KindSignatures #-}
module Generated (PacketT(..), workM, viewsM) where

import Data.Functor.Const (Const, getConst)
import Data.Kind (Type)
import Data.Void (Void, absurd)
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

data PacketT (f :: Type -> Type) = PacketT (M.Th Int) (M.Th (M.MaybeT (f Int)))

mapThunk :: (a -> b) -> M.Th a -> M.Th b
mapThunk f (M.Th name body annotation) = M.Th name (f body) annotation

emptyView :: M.MaybeT (Const Void Int) -> M.MaybeT (f Int)
emptyView M.NothingT = M.NothingT
emptyView (M.JustT leaf) = M.JustT (mapThunk (absurd . getConst) leaf)

packetView :: M.Th (PacketT (Const Void)) -> M.Th (PacketT f)
packetView = mapThunk convert
  where
    convert :: PacketT (Const Void) -> PacketT f
    convert (PacketT fixed optional) = PacketT fixed (mapThunk emptyView optional)

workM :: M.Th Int -> M.MemoristM Int
workM count = do
    M.tick
    n <- M.force count
    pure (n + n)

-- Entries: views = 1; work = 1 on the first fixed-field demand.
-- Skip/roots/optional = 1; first/full/cached full = 2; four fresh full = 8.
-- Paid count: roots/optional = 1, full = 3. The paid t is never forced.
-- Both higher-kinded views retain the one root and its two field identities.
viewsM :: M.Th t -> M.Th Int -> M.MemoristM (M.PairT (PacketT L.ListT) (PacketT M.MaybeT))
viewsM _ count = do
    M.tick
    root <- M.lazy $ do
        fixed <- M.lazy (workM count)
        optional <- M.lazy (pure M.NothingT)
        pure (PacketT fixed optional)
    pure (M.PairT (packetView root) (packetView root))
