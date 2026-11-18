module Generated (PacketT(..), viewsM) where

import Data.Void (Void, absurd)
import qualified Memorist.Memorist as M

data PacketT a = PacketT (M.Th Int) (M.Th (M.MaybeT a))

-- The source constructs no polymorphic leaves. Void makes that fact explicit;
-- these total views preserve native thunk metadata rather than allocating again.
mapThunk :: (a -> b) -> M.Th a -> M.Th b
mapThunk f (M.Th name body annotation) = M.Th name (f body) annotation

emptyView :: M.MaybeT Void -> M.MaybeT a
emptyView M.NothingT = M.NothingT
emptyView (M.JustT leaf) = M.JustT (mapThunk absurd leaf)

packetView :: M.Th (PacketT Void) -> M.Th (PacketT a)
packetView = mapThunk convert
  where
    convert (PacketT fixed optional) = PacketT fixed (mapThunk emptyView optional)

viewsM :: M.Th t -> M.Th Int -> M.MemoristM (M.PairT (PacketT Int) (PacketT Bool))
viewsM _ count = do
    M.tick
    root <- M.lazy $ do
        fixed <- M.lazy $ do
            M.tick
            n <- M.force count
            pure (n + n)
        optional <- M.lazy (pure M.NothingT)
        pure (PacketT fixed optional)
    pure (M.PairT (packetView root) (packetView root))
