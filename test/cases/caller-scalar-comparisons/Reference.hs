module Generated where

import qualified Memorist.Memorist as M

-- A source function entry costs one. The selected comparison itself costs
-- zero; forcing the supplied operands preserves their independently priced work.
binary :: (a -> a -> Bool) -> M.Th a -> M.Th a -> M.MemoristM Bool
binary relation x y = do
    M.tick
    a <- M.force x
    b <- M.force y
    pure (relation a b)

equalM, differentM :: Eq a => M.Th a -> M.Th a -> M.MemoristM Bool
equalM = binary (==)
differentM = binary (/=)

lessM, lessEqualM, greaterM, greaterEqualM, equalViaOrdM ::
    Ord a => M.Th a -> M.Th a -> M.MemoristM Bool
lessM = binary (<)
lessEqualM = binary (<=)
greaterM = binary (>)
greaterEqualM = binary (>=)
equalViaOrdM = binary (==)

skippedM :: Eq a => M.Th Bool -> M.Th a -> M.Th a -> M.MemoristM Bool
skippedM enabled x y = do
    M.tick
    wanted <- M.force enabled
    if wanted then do
        a <- M.force x
        b <- M.force y
        pure (a == b)
    else pure False

suspendedM :: Eq a => M.Th a -> M.Th a -> M.MemoristM (M.MaybeT Bool)
suspendedM x y = do
    M.tick
    M.JustT <$> M.lazy (do
        a <- M.force x
        b <- M.force y
        pure (a == b))

-- Only work n and (when n /= 0) work 0 are entered. This reference counts
-- those entries directly instead of mirroring the generated recursion.
workM :: M.Th Int -> M.MemoristM Int
workM input = do
    M.tick
    n <- M.force input
    if n == 0 then pure 1 else M.tick >> pure 2
