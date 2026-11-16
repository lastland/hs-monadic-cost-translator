{-# LANGUAGE RankNTypes #-}
module Generated where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

emptyM :: M.Th a -> M.MemoristM (L.ListT b)
emptyM _ = M.tick >> pure L.NilT

intsM :: M.Th (L.ListT Int) -> M.MemoristM Int
intsM xs = do
    M.tick
    list <- M.force xs
    case list of
        L.NilT -> pure 5
        L.ConsT _ _ -> error "ints: nonempty list"

boolsM :: M.Th (L.ListT Bool) -> M.MemoristM Int
boolsM xs = do
    M.tick
    list <- M.force xs
    case list of
        L.NilT -> pure 5
        L.ConsT _ _ -> error "bools: nonempty list"

localM :: M.Th a -> M.MemoristM Int
localM x = do
    M.tick
    anchor <- M.lazy (pure ())
    -- The polymorphic source value is one thunk, not one per instantiation.
    -- Reuse its reserved allocator name at each element type.
    let shared :: forall b. M.Th (L.ListT b)
        shared = M.value (M.eval (M.lazy (emptyM x)) (M.tname anchor))
    a <- intsM shared
    b <- boolsM shared
    pure (a + b)
