module Generated (viewsM, nestedM, distinctM) where

import qualified Memorist.Memorist as M

-- Independent finite reference: one native shared root stores a Maybe of unit
-- tokens. Each token denotes a source bottom; observations either skip it or
-- fail. Int and Bool views retain the same runtime metadata, not separate roots.
copyThunk :: (a -> b) -> M.Th a -> M.Th b
copyThunk f (M.Th name value annotation) = M.Th name (f value) annotation

view :: M.Th (M.MaybeT ()) -> M.Th (M.MaybeT a)
view = copyThunk convert
  where
    convert M.NothingT = M.NothingT
    convert (M.JustT token) = M.JustT (copyThunk (const (error "producer payload failure")) token)

nestedView :: M.Th (M.MaybeT (M.MaybeT ())) -> M.Th (M.MaybeT (M.MaybeT a))
nestedView = copyThunk convert
  where
    convert M.NothingT = M.NothingT
    convert (M.JustT inside) = M.JustT (view inside)

producer :: M.Th Int -> M.MemoristM (M.MaybeT ())
producer count = do
    M.tick
    n <- M.force count
    if n < 0 then pure (error "producer root failure") else
        if n == 0 then pure M.NothingT else do
            token <- M.lazy (pure ())
            pure (M.JustT token)

nestedProducer :: M.Th Int -> M.MemoristM (M.MaybeT (M.MaybeT ()))
nestedProducer count = do
    M.tick
    n <- M.force count
    if n < 0 then pure M.NothingT else do
        inside <- M.lazy (producer count)
        pure (M.JustT inside)

viewsM :: M.Th t -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT Int) (M.MaybeT Bool))
viewsM _ count = do
    M.tick
    shared <- M.lazy (M.tick >> producer count) -- gate ignores the captured input
    pure (M.PairT (view shared) (view shared))

nestedM :: M.Th t -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT (M.MaybeT Int)) (M.MaybeT (M.MaybeT Bool)))
nestedM _ count = do
    M.tick
    shared <- M.lazy (M.tick >> nestedProducer count)
    pure (M.PairT (nestedView shared) (nestedView shared))

distinctM :: M.Th t -> M.Th Int -> M.MemoristM (M.PairT (M.MaybeT Int) (M.MaybeT Bool))
distinctM _ count = do
    M.tick
    first <- M.lazy (M.tick >> producer count)
    second <- M.lazy (M.tick >> producer count)
    pure (M.PairT (view first) (view second))
