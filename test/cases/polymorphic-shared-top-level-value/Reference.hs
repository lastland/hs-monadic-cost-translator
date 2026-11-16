module Generated (Bindings, initialize, intsM, boolsM) where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

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

intsM :: Bindings -> M.Th Int -> M.MemoristM Int
intsM (Bindings shared) argument = do
    M.tick
    list <- M.force shared
    case list of
        L.NilT -> M.force argument
        L.ConsT _ _ -> M.force argument

boolsM :: Bindings -> M.Th Bool -> M.MemoristM Bool
boolsM (Bindings shared) argument = do
    M.tick
    list <- M.force shared
    case list of
        L.NilT -> M.force argument
        L.ConsT _ _ -> M.force argument
