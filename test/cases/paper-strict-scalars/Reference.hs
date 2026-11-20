module Generated where

import Data.Maybe (fromMaybe)
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

-- The flag selects whether to pay for x. Once entered, the path uses
-- that value for both its predicate(s) and result.
afterAndM, afterOrM, nestedAndM :: M.Th Bool -> M.Th Int -> M.MemoristM Int
afterAndM = whenFlag True (\n -> if n > 0 then n + 1 else 0)
afterOrM = whenFlag False (\n -> if n <= 0 then n - 1 else 0)
nestedAndM = whenFlag True (\n -> if n `elem` [1..7] then n + 2 else 0)

whenFlag :: Bool -> (Int -> Int) -> M.Th Bool -> M.Th Int -> M.MemoristM Int
whenFlag entered result flag x = do
    M.tick
    actual <- M.force flag
    if actual == entered then result <$> M.force x else pure 0

bangNextM :: M.Th Int -> M.MemoristM Int
bangNextM x = M.tick >> M.tick >> ((\n -> double n + 1) <$> M.force x)

letGuardM, variableGuardM :: M.Th Int -> M.MemoristM Int
letGuardM x = M.tick >> ((\n -> if n <= 0 then pred n else n) <$> M.force x)
variableGuardM = letGuardM

checkedFlagM :: M.Th Bool -> M.MemoristM Bool
checkedFlagM flag = M.tick >> M.force flag

aliasGuardM :: M.Th Bool -> M.Th Int -> M.MemoristM (M.MaybeT Int)
aliasGuardM enabled x = do
    M.tick
    check <- M.force enabled
    if check then do
        n <- M.force x
        n `seq` pure (M.JustT x)
    else pure (M.JustT x)

literalM, inspectM :: M.Th Int -> M.MemoristM Int
literalM x = M.tick >> ((\n -> fromMaybe 0 (lookup n [(0,1), (1,3)])) <$> M.force x)
inspectM x = M.tick >> ((\n -> if n == 0 then 1 else n - 1) <$> M.force x)

zeroSignM :: M.Th Double -> M.MemoristM Int
zeroSignM input = M.tick >> (sign <$> M.force input)
  where
    sign n | isNegativeZero n = -1
           | n == 0 = 1
           | otherwise = 0

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

viaCaseM :: M.Th Int -> M.MemoristM Int
viaCaseM x = M.tick >> (double <$> M.force x)

nestedM :: M.Th Int -> M.MemoristM Int
nestedM x = M.tick >> (result <$> M.force x)
  where
    result n | n + 1 <= 0 = n
             | otherwise = n + 2

caseRowsM :: M.Th Int -> M.Th Bool -> M.MemoristM Int
caseRowsM x _ = M.tick >> ((\n -> if n <= 0 then pred n else 0) <$> M.force x)

equationM :: M.Th Int -> M.Th Int -> M.MemoristM Int
equationM first second = do
    M.tick
    n <- M.force first
    if n > 0 then M.force second else (n -) <$> M.force second

joinRowsM :: M.Th Int -> M.Th Bool -> M.MemoristM (M.MaybeT Int)
joinRowsM x b = do
    M.tick
    check <- M.force b
    stop <- if check then (> 0) <$> M.force x else pure False
    if stop then pure M.NothingT else M.JustT <$> M.lazy (pred <$> M.force x)

data PacketT a = PacketT (M.Th Int) (M.Th (M.MaybeT a))
type Views = M.PairT (PacketT Bool) (PacketT Int)

-- The two types observe the same root and fields. Only Nothing inhabits
-- the optional field in this source, so its view needs no leaf conversion.
payloadM :: M.Th Int -> M.MemoristM Views
payloadM = sharedPacket (pure . (+ 1))

payloadWorkM :: M.Th Int -> M.Th Int -> M.MemoristM Views
payloadWorkM x y = sharedPacket (\n -> M.tick >> (double . (n +) <$> M.force y)) x

sharedPacket :: (Int -> M.MemoristM Int) -> M.Th Int -> M.MemoristM Views
sharedPacket whenPositive x = do
    M.tick
    root <- M.lazy $ do
        n <- M.force x
        let selected | n > 0 = whenPositive n
                     | otherwise = pure 0
        n `seq` (PacketT <$> M.lazy selected <*> M.lazy (pure M.NothingT))
    pure (M.PairT root (view root))
  where
    view (M.Th key packet annotation) = M.Th key (convert packet) annotation
    convert (PacketT fixed (M.Th key _ annotation)) =
        PacketT fixed (M.Th key M.NothingT annotation)
