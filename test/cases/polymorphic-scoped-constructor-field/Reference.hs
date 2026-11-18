module Generated (PacketT(..), viewsM) where

import Data.Void (Void, absurd)
import qualified Memorist.Memorist as M

data PacketT a = PacketT (M.Th Int) (M.Th (M.MaybeT a))

mapThunk :: (a -> b) -> M.Th a -> M.Th b
mapThunk f (M.Th name body annotation) = M.Th name (f body) annotation

emptyView :: M.MaybeT Void -> M.MaybeT a
emptyView M.NothingT = M.NothingT
emptyView (M.JustT leaf) = M.JustT (mapThunk absurd leaf)

packetView :: M.Th (PacketT Void) -> M.Th (PacketT a)
packetView = mapThunk convert
  where
    convert (PacketT fixed optional) = PacketT fixed (mapThunk emptyView optional)

-- One function-entry tick. The fixed field suspends the source's marker/case;
-- neither the local marker nor the case enters a source function.
viewsM :: M.Th t -> M.Th Int -> M.MemoristM (M.PairT (PacketT Int) (PacketT Bool))
viewsM _ _ = do
    M.tick
    root <- M.lazy $ do
        fixed <- M.lazy $ do
            marker <- M.lazy (pure M.NothingT)
            value <- M.force marker
            pure (case value of M.NothingT -> 1; M.JustT _ -> 2)
        optional <- M.lazy (pure M.NothingT)
        pure (PacketT fixed optional)
    pure (M.PairT (packetView root) (packetView root))
