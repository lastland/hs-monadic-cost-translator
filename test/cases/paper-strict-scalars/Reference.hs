module Generated where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

-- Count an entry for each observed recursive call, including the terminating
-- call. The count itself can be carried as an Int; the input spine stays lazy.
takeM :: M.Th Int -> M.Th (L.ListT a) -> M.MemoristM (L.ListT a)
takeM count xs = do
    M.tick
    n <- M.force count
    prefix n xs
  where
    prefix n _ | n <= 0 = pure L.NilT
    prefix n input = do
        spine <- M.force input
        case spine of
            L.NilT -> pure L.NilT
            L.ConsT h t -> L.ConsT h <$> M.lazy (M.tick >> prefix (n - 1) t)

workM :: M.Th Int -> M.MemoristM Int
workM z = M.tick >> (double <$> M.force z)

double :: Int -> Int
double = (2 *)

-- The field's single tick is the source call to work. With input 3 the
-- projection is Just 8, costing 1 tick for the tag and 2 for the full value.
boxNextM, boxPrefixM :: M.Th Int -> M.MemoristM (M.MaybeT Int)
boxNextM x = do
    M.tick
    n <- M.force x
    if n <= 0 then pure M.NothingT
    else M.JustT <$> M.lazy (M.tick >> pure (double (n + 1)))
boxPrefixM = boxNextM

boxQuotM :: M.Th Bool -> M.Th Int -> M.MemoristM (M.MaybeT Int)
boxQuotM use x = do
    M.tick
    n <- M.force x
    if n < 0 then pure M.NothingT
    else M.JustT <$> M.lazy (do
        M.tick
        wanted <- M.force use
        pure (if wanted then 12 `div` n else 7))

guardedM :: M.Th Bool -> M.Th Int -> M.MemoristM (M.MaybeT Int)
guardedM check x = do
    M.tick
    enabled <- M.force check
    stop <- if enabled then (<= 0) <$> M.force x else pure False
    if stop then pure M.NothingT
    else M.JustT <$> M.lazy (do
        M.tick
        n <- M.force x
        pure (double (n + 1)))

-- The two fields independently depend on x. Asking for both pays for the
-- same input thunk once, but pays for each source call (add or work) reached.
suspendedM :: M.Th Int -> M.Th Int -> M.MemoristM (M.PairT Int Int)
suspendedM x y = do
    M.tick
    left <- M.lazy $ do
        n <- M.force x
        if n <= 0 then pure 0 else M.tick >> ((n +) <$> M.force y)
    right <- M.lazy $ do
        M.tick
        n <- M.force x
        pure (3 + double (n - 1))
    pure (M.PairT left right)

shadowForwardM :: M.Th Int -> M.Th (M.MaybeT Int) -> M.MemoristM (M.MaybeT Int)
shadowForwardM outer input = do
    M.tick
    n <- M.force outer
    if n <= 0 then relay outer else do
        item <- M.force input
        case item of
            M.NothingT -> pure M.NothingT
            M.JustT inner -> do
                m <- M.force inner
                if m < 0 then relay inner else pure M.NothingT
  where
    relay original = M.tick >> pure (M.JustT original)

mappedGuardsM :: M.Th Int -> M.Th Int -> M.MemoristM Int
mappedGuardsM x y = do
    M.tick
    a <- M.force x
    if a <= 0 then pure 0 else do
        b <- M.force y
        pure (if b > 0 then a - b else 0)

-- The outer function and its local operator each charge one entry tick.
shadowPlusM :: M.Th Int -> M.MemoristM Int
shadowPlusM x = M.tick >> M.tick >> M.force x

givenAddM :: M.Th Int -> M.MemoristM (M.MaybeT Int)
givenAddM x = M.tick >> (M.JustT <$> M.lazy ((+ 1) <$> M.force x))
