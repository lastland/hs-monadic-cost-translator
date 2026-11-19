module Generated (PacketT(..), viewsM) where

import Data.Void (Void, absurd)
import qualified Memorist.Memorist as M

data PacketT a = PacketT (M.Th Int) (M.Th (M.MaybeT a))

mapThunk :: (a -> b) -> M.Th a -> M.Th b
mapThunk f (M.Th name body annotation) = M.Th name (f body) annotation

packetView :: PacketT Void -> PacketT a
packetView (PacketT fixed optional) = PacketT fixed (mapThunk emptyView optional)
  where
    emptyView M.NothingT = M.NothingT
    emptyView (M.JustT leaf) = M.JustT (mapThunk absurd leaf)

-- One invocation of views enters choose once. make enters once for True and
-- twice for False. Demanding the fixed field enters work once; optional never
-- enters work. Thus full costs are 4/5, roots and optional cost 3/4, and skipping
-- the result costs 1. Paid flag/count each add one only when demanded.
viewsM :: M.Th t -> M.Th Bool -> M.Th Int -> M.MemoristM (M.PairT (PacketT Int) (PacketT Bool))
viewsM _ flag count = do
    M.tick
    root <- M.lazy $ do
        M.tick
        make flag count
    pure (M.PairT (mapThunk packetView root) (mapThunk packetView root))
  where
    make :: M.Th Bool -> M.Th Int -> M.MemoristM (PacketT Void)
    make choice input = do
        M.tick
        selected <- M.force choice
        if selected then do
            fixed <- M.lazy $ do
                M.tick
                n <- M.force input
                pure (n + n)
            optional <- M.lazy (pure M.NothingT)
            pure (PacketT fixed optional)
        else do
            yes <- M.lazy (pure True)
            next <- M.lazy $ do
                n <- M.force input
                pure (n + 1)
            make yes next
