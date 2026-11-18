module Generated (PacketT(..), viewsM) where

import Data.Void (Void, absurd)
import qualified Memorist.Memorist as M

data PacketT a = PacketT (M.Th Int) (M.Th (M.MaybeT a))

-- No branch constructs a polymorphic leaf. These total views preserve the
-- native root/field names and annotations, without new thunk allocations.
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

-- Worked entry costs: views = 1; first root demand runs choose = 1 and
-- forces count; first fixed-field demand runs work = 1. Arithmetic is free.
-- Thus skip = 1, roots/optional only = 2, full = 3, four fresh full calls
-- = 12, and repeated full demands on one cached call = 3. A paid count adds
-- exactly 1 on the first root demand (paid roots = 3, paid full = 4).
-- The unused t argument is never forced. Both branches allocate fields only
-- after the one ordinary root action has selected a constructor.
viewsM :: M.Th t -> M.Th Int -> M.MemoristM (M.PairT (PacketT Int) (PacketT Bool))
viewsM _ count = do
    M.tick
    root <- M.lazy $ do
        M.tick
        n <- M.force count
        let selected = if n < 0 then n else n + 1
        fixed <- M.lazy $ do
            M.tick
            pure (selected + selected)
        optional <- M.lazy (pure M.NothingT)
        pure (PacketT fixed optional)
    pure (M.PairT (packetView root) (packetView root))
