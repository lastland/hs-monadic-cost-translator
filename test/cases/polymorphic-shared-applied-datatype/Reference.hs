module Generated (PacketT(..), keepM, workM, viewsM) where

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
    convert :: PacketT Void -> PacketT a
    convert (PacketT fixed optional) = PacketT fixed (mapThunk emptyView optional)

keepM :: M.Th a -> M.MemoristM a
keepM argument = M.tick >> M.force argument

workM :: M.Th Int -> M.MemoristM Int
workM count = do
    M.tick
    n <- M.force count
    pure (n + n)

-- Entry ticks: views = 1, keep = 1 on first root demand, work = 1 on
-- first fixed-field demand. The constructor and keep do not force count.
-- Skip = 1; roots/optional = 2; first field/full = 3; cached full = 3;
-- four fresh full calls = 12. Paid count: roots/optional = 2, full = 4.
-- The paid, unused t input is never forced. One ordinary shared root owns
-- one constructor argument thunk, allocated inside that root's lazy action.
viewsM :: M.Th t -> M.Th Int -> M.MemoristM (M.PairT (PacketT Int) (PacketT Bool))
viewsM _ count = do
    M.tick
    root <- M.lazy $ do
        argument <- M.lazy $ do
            fixed <- M.lazy (workM count)
            optional <- M.lazy (pure M.NothingT)
            pure (PacketT fixed optional)
        keepM argument
    pure (M.PairT (packetView root) (packetView root))
