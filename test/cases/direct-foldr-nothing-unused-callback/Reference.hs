module Generated where

import Memorist.Memorist

addM :: Th a -> Th b -> MemoristM c
addM _ _ = do
    tick
    pure undefined

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

foldedM :: Th (MaybeT a) -> MemoristM Int
foldedM input = do
    tick
    callback <- lazy (pure addM)
    five <- lazy (pure 5)
    foldrMaybeM callback five input
