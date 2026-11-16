{-# LANGUAGE TypeFamilies #-}
-- Handwritten from Example.hs. A source call ticks; allocating or forcing a
-- thunk does not. Variable matches preserve aliases, while a source let or a
-- computed match subject introduces its own lazy binding.
module Generated where

import qualified Example as Source
import qualified Memorist.Memorist as M

data BoxT = BoxT (M.Th Int)
data BoxD = BoxD (M.ThD Int)
data RecordT = RecordT (M.Th Int) (M.Th Int)
data RecordD = RecordD (M.ThD Int) (M.ThD Int)

instance M.Repr Source.Box where
    type Rep Source.Box = BoxT
    reprM (Source.Box n) = BoxT <$> M.lazy (M.reprM n)
    abstract (BoxT n) = Source.Box (M.tval n)

instance M.Repr Source.Record where
    type Rep Source.Record = RecordT
    reprM (Source.Record a b) =
        RecordT <$> M.lazy (M.reprM a) <*> M.lazy (M.reprM b)
    abstract (RecordT a b) = Source.Record (M.tval a) (M.tval b)

instance M.Demand BoxT where
    type Dem BoxT = BoxD
    fullDemand (BoxT n) = BoxD (M.fullDemand n)
    usage (BoxT n) (BoxD d) = M.usage n d

instance M.Demand RecordT where
    type Dem RecordT = RecordD
    fullDemand (RecordT a b) = RecordD (M.fullDemand a) (M.fullDemand b)
    usage (RecordT a b) (RecordD da db) = M.usage a da `M.union` M.usage b db

workM :: M.Th Int -> M.MemoristM Int
workM x = do
    M.tick
    n <- M.force x
    pure (n + n)

badM :: M.Th a -> M.MemoristM a
badM _ = do
    M.tick
    error "demanded bad computation"

caseComputedM, guardComputedM, caseConditionalM, guardConditionalM,
    caseBadM, guardBadM, sourceLetM ::
    M.Th Int -> M.MemoristM (M.PairT Int Int)
caseComputedM x = do
    M.tick
    y <- M.lazy (workM x)
    pure (M.PairT y y)
guardComputedM x = do
    M.tick
    y <- M.lazy (workM x)
    pure (M.PairT y y)
caseConditionalM x = do
    M.tick
    y <- M.lazy (if (undefined :: Bool) then M.force x else M.force x)
    pure (M.PairT y y)
guardConditionalM x = do
    M.tick
    y <- M.lazy (if (undefined :: Bool) then M.force x else M.force x)
    pure (M.PairT y y)
caseBadM x = do
    M.tick
    y <- M.lazy (badM x)
    pure (M.PairT y y)
guardBadM x = do
    M.tick
    y <- M.lazy (badM x)
    pure (M.PairT y y)
sourceLetM x = do
    M.tick
    y <- M.lazy (M.force x)
    pure (M.PairT y y)

caseVariableM, caseWildcardM, guardVariableM, guardWildcardM,
    caseLiteralM, guardLiteralM :: M.Th Int -> M.MemoristM Int
caseVariableM _ = M.tick >> pure 7
caseWildcardM _ = M.tick >> pure 7
guardVariableM _ = M.tick >> pure 7
guardWildcardM _ = M.tick >> pure 7
caseLiteralM x = do
    M.tick
    n <- M.force x
    pure (if n == 0 then 7 else 8)
guardLiteralM x = do
    M.tick
    n <- M.force x
    pure (if n == 0 then 7 else 8)

caseConstructorM, guardConstructorM :: M.Th BoxT -> M.MemoristM Int
caseConstructorM x = do
    M.tick
    box <- M.force x
    case box of BoxT _ -> pure 7
guardConstructorM x = do
    M.tick
    box <- M.force x
    case box of BoxT _ -> pure 7

caseAsM, guardAsM :: M.Th BoxT -> M.MemoristM (M.PairT BoxT BoxT)
caseAsM x = do
    M.tick
    box <- M.force x
    case box of BoxT _ -> pure (M.PairT x x)
guardAsM x = do
    M.tick
    box <- M.force x
    case box of BoxT _ -> pure (M.PairT x x)

-- Matching follows the source pattern from left to right. In particular, a
-- failed first literal must not inspect the second box, even its constructor.
caseNestedM, guardNestedM ::
    M.Th (M.PairT BoxT BoxT) -> M.MemoristM Int
caseNestedM x = M.tick >> matchNested x
guardNestedM x = M.tick >> matchNested x

matchNested :: M.Th (M.PairT BoxT BoxT) -> M.MemoristM Int
matchNested x = do
    pair <- M.force x
    case pair of
        M.PairT a b -> do
            box <- M.force a
            case box of
                BoxT field -> do
                    n <- M.force field
                    if n /= 0 then pure 2 else do
                        box' <- M.force b
                        case box' of
                            BoxT field' -> do
                                m <- M.force field'
                                pure (if m == 0 then 1 else 2)

-- The record pattern explicitly names second before first. Declaration order
-- must not replace this matching order.
caseRecordM, guardRecordM :: M.Th RecordT -> M.MemoristM Int
caseRecordM x = M.tick >> matchRecord x
guardRecordM x = M.tick >> matchRecord x

matchRecord :: M.Th RecordT -> M.MemoristM Int
matchRecord x = do
    record <- M.force x
    case record of
        RecordT a b -> do
            second <- M.force b
            if second /= 0 then pure 2 else do
                first <- M.force a
                pure (if first == 0 then 1 else 2)

caseGuardSequenceM, guardSequenceM :: M.Th Int -> M.MemoristM Int
caseGuardSequenceM x = M.tick >> matchGuards x
guardSequenceM x = M.tick >> matchGuards x

matchGuards :: M.Th Int -> M.MemoristM Int
matchGuards x = do
    n <- M.force x
    if n > 0 then do
        badResult <- badM x
        pure (if badResult == 0 then 1 else 2)
    else pure 2

ignoreM :: M.Th Int -> M.Th Int -> M.MemoristM Int
ignoreM _ x = M.tick >> M.force x

conditionalArgM, nestedLetArgM, scalarM, scalarWorkM, primitiveWorkM,
    conditionalWorkM :: M.Th Int -> M.MemoristM Int
conditionalArgM x = do
    M.tick
    unused <- M.lazy (if (undefined :: Bool) then M.force x else M.force x)
    ignoreM unused x
nestedLetArgM x = do
    M.tick
    unused <- M.lazy $ do
        y <- M.lazy (badM x)
        a <- M.force y
        b <- M.force y
        pure (a + b)
    ignoreM unused x
scalarM x = M.tick >> M.force x
scalarWorkM x = do
    M.tick
    y <- M.lazy (workM x)
    scalarM y
primitiveWorkM x = do
    M.tick
    a <- workM x
    next <- M.lazy $ do
        n <- M.force x
        pure (n + 1)
    b <- workM next
    pure (a + b)
conditionalWorkM x = do
    M.tick
    condition <- M.lazy (positiveM x)
    yes <- M.lazy (workM x)
    no <- M.lazy (badM x)
    chooseM condition yes no

addM :: M.Th Int -> M.Th Int -> M.MemoristM Int
addM x y = do
    M.tick
    a <- M.force x
    b <- M.force y
    pure (a + b)

chooseM :: M.Th Bool -> M.Th Int -> M.Th Int -> M.MemoristM Int
chooseM condition yes no = do
    M.tick
    selected <- M.force condition
    if selected then M.force yes else M.force no

positiveM :: M.Th Int -> M.MemoristM Bool
positiveM x = do
    M.tick
    n <- M.force x
    pure (n > 0)

boolAndM, boolOrM :: M.Th Bool -> M.Th Bool -> M.MemoristM Bool
boolAndM x y = do
    M.tick
    a <- M.force x
    if a then M.force y else pure False
boolOrM x y = do
    M.tick
    a <- M.force x
    if a then pure True else M.force y

shortAndM, shortOrM :: M.Th Int -> M.MemoristM Bool
shortAndM x = do
    M.tick
    left <- M.lazy (positiveM x)
    right <- M.lazy $ do
        y <- M.lazy (workM x)
        positiveM y
    boolAndM left right
shortOrM x = do
    M.tick
    left <- M.lazy (positiveM x)
    right <- M.lazy $ do
        y <- M.lazy (workM x)
        positiveM y
    boolOrM left right
