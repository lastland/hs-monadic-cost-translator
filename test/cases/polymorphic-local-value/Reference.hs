module Generated where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import Unsafe.Coerce (unsafeCoerce)

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
    shared <- M.lazy (emptyM x :: M.MemoristM (L.ListT ()))
    -- The polymorphic source value is one thunk, not one per instantiation.
    -- Its only possible constructor is NilT, so no element is coerced or read.
    a <- intsM (unsafeCoerce shared)
    b <- boolsM (unsafeCoerce shared)
    pure (a + b)
