module Generated where

import Memorist.Memorist

differenceM :: Num a => Th a -> Th a -> MemoristM a
differenceM tx ty = do
    tick
    x <- force tx
    y <- force ty
    pure (x - y)

foldlPairM :: Th (Th b -> Th a -> MemoristM b)
           -> Th b -> Th (PairT ignored a) -> MemoristM b
foldlPairM callback zero input = do
    tick
    pair <- force input
    case pair of
        PairT _ value -> do
            combine <- force callback
            combine zero value

foldedM :: Num a => Th (PairT ignored a) -> MemoristM a
foldedM input = do
    tick
    callback <- lazy (pure differenceM)
    ten <- lazy (pure 10)
    foldlPairM callback ten input
