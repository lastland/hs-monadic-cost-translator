module Generated where

import Memorist.Memorist
import Memorist.Data.List (ListT(..))
import qualified Memorist.Example.List as ML

singleM :: Th a -> MemoristM (ListT a)
singleM tx = do
    tick
    end <- lazy (pure NilT)
    pure (ConsT tx end)

foldrMaybeM :: Th (Th a -> Th b -> MemoristM b)
             -> Th b -> Th (MaybeT a) -> MemoristM b
foldrMaybeM callback zero input = do
    tick
    container <- force input
    case container of
        NothingT -> force zero
        JustT value -> do
            combine <- force callback
            combine value zero

concatMapMaybeM :: Th (Th a -> MemoristM (ListT b))
                 -> Th (MaybeT a) -> MemoristM (ListT b)
concatMapMaybeM callback input = do
    tick
    -- Preserve the runtime's concatMap = foldr ((++) . f) [] algorithm.
    -- Even a singleton container must not erase the fold and append calls.
    makeList <- force callback
    let appendResult value rest = do
            produced <- lazy (makeList value)
            ML.appendM produced rest
    combine <- lazy (pure appendResult)
    end <- lazy (pure NilT)
    foldrMaybeM combine end input

flatM :: Th (MaybeT a) -> MemoristM (ListT a)
flatM input = do
    tick
    callback <- lazy (pure singleM)
    concatMapMaybeM callback input

observedM :: Th (MaybeT a) -> MemoristM a
observedM input = do
    tick
    flattened <- lazy (flatM input)
    ML.headM flattened
