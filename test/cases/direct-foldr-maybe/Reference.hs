module Generated where

import Memorist.Memorist

differenceM :: Num a => Th a -> Th a -> MemoristM a
differenceM tx ty = do
    tick
    x <- force tx
    y <- force ty
    pure (x - y)

-- A direct Foldable operation: inspect the container, not an intermediate list.
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

foldedM :: Num a => Th (MaybeT a) -> MemoristM a
foldedM input = do
    tick
    callback <- lazy (pure differenceM)
    ten <- lazy (pure 10)
    foldrMaybeM callback ten input
