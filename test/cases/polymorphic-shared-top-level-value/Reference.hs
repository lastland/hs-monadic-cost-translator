module Generated (intsM, boolsM) where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

-- Keep source-global sharing private to the reasoning model. The enclosing
-- thunk gives every global a negative ancestry, disjoint from caller roots.
globalBindings :: Bindings
globalBindings = M.tval (M.value (M.eval (M.lazy initialize) (M.fromInt (-1))))

-- Only the empty constructor is observed. Its element type is irrelevant,
-- so one ordinary, type-safe thunk can serve both exported scalar facades.
newtype Bindings = Bindings (M.Th (L.ListT ()))

initialize :: M.MemoristM Bindings
initialize = Bindings <$> M.lazy (do
    five <- M.lazy (pure 5)
    emptyM five)

emptyM :: M.Th Int -> M.MemoristM (L.ListT ())
emptyM argument = do
    M.tick
    n <- M.force argument
    pure (if n > 0 then L.NilT else L.NilT)

intsM :: M.Th Int -> M.MemoristM Int
intsM = intsWithBindings globalBindings

intsWithBindings :: Bindings -> M.Th Int -> M.MemoristM Int
intsWithBindings (Bindings shared) argument = do
    M.tick
    list <- M.force shared
    case list of
        L.NilT -> M.force argument
        L.ConsT _ _ -> M.force argument

boolsM :: M.Th Bool -> M.MemoristM Bool
boolsM = boolsWithBindings globalBindings

boolsWithBindings :: Bindings -> M.Th Bool -> M.MemoristM Bool
boolsWithBindings (Bindings shared) argument = do
    M.tick
    list <- M.force shared
    case list of
        L.NilT -> M.force argument
        L.ConsT _ _ -> M.force argument
