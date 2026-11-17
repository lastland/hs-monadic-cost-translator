module Case (probes) where

import Control.Monad (replicateM)
import Data.List (nub)
import qualified Example as Source
import Generated (viewsM, inferredM, distinctM)
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "views/bool-unused-input"
        (sourceViews (undefined :: Bool) . number)
        (observeViews (undefined :: Bool) . number)
    , scalarSource "views/function-unused-input"
        (sourceViews (undefined :: Int -> Int) . number)
        (observeViews (undefined :: Int -> Int) . number)
    , scalarSource "views/nonnegative"
        (sourceViews (undefined :: Bool) . safeCount)
        (observeViews (undefined :: Bool) . safeCount)
    , scalarSource "views/two-fresh-invocations"
        ((2 *) . sourceViews (undefined :: Bool) . safeCount)
        (freshViews 2 . safeCount)
    , scalarSource "views/repeated-fresh-invocations"
        (\sample -> repeats sample * sourceViews (undefined :: Bool) (safeCount sample))
        (\sample -> freshViews (repeats sample) (safeCount sample))
    , scalarSource "views/cached-call-repeated-demands"
        (\sample -> repeats sample * sourceViews (undefined :: Bool) (safeCount sample)) $ \sample -> do
        cached <- M.lazy (callViews (undefined :: Bool) (safeCount sample))
        sum <$> replicateM (repeats sample) (M.force cached >>= forceSum)
    , scalarSource "inferred/outer-int-bottom"
        (const (sourceInferred (undefined :: Int)))
        (const (typedThunk (undefined :: Int) >>= generic))
    , scalarSource "inferred/outer-integer-bottom"
        (const (sourceInferred (undefined :: Integer)))
        (const (typedThunk (undefined :: Integer) >>= generic))
    , scalarSource "inferred/outer-int-scope"
        (sourceInferred . number)
        (\sample -> typedThunk (number sample) >>= generic)
    , scalarSource "inferred/outer-integer-scope"
        (sourceInferred . toInteger . number)
        (\sample -> typedThunk (toInteger (number sample)) >>= generic)
    , scalarSource "distinct/source-bindings"
        (const (sourceSum (Source.distinct (undefined :: Bool))))
        (const (typedThunk (undefined :: Bool) >>= distinctM >>= forceSum))
    , demanded "views/selected-dictionaries" projectViews demandViews
        (callViews (undefined :: Bool) . safeCount)
    , demanded "inferred/selected-dictionaries" projectViews demandViews
        (const (typedThunk (undefined :: Int) >>= inferredM))
    , demanded "distinct/selected-dictionaries" projectViews demandViews
        (const (typedThunk (undefined :: Bool) >>= distinctM))
    , demanded "views/selected-single-aliases" projectSingle demandSingle
        (callViews (undefined :: Bool) . safeCount)
    , demanded "views/bottom-count-skipped" (\_ _ -> ()) (\_ _ -> M.PairD M.Skip M.Skip)
        (const (callViews (undefined :: Bool) (undefined :: Int)))
    , demanded "views/negative-count-skipped" (\_ _ -> ()) (\_ _ -> M.PairD M.Skip M.Skip)
        (const (callViews (undefined :: Bool) (-1)))
    , demanded "views/outer-pairs-only-bottom-count" (\_ _ -> ())
        (\_ _ -> M.PairD (M.Need (M.PairD M.Skip M.Skip)) (M.Need (M.PairD M.Skip M.Skip)))
        (const (callViews (undefined :: Bool) (undefined :: Int)))
    , failure "views/demanded-negative-int"
        (const (callViews (undefined :: Bool) (-1) >>= firstInt))
    , failure "views/demanded-negative-integer"
        (const (callViews (undefined :: Bool) (-1) >>= secondInteger))
    , failure "views/demanded-bottom-count"
        (const (callViews (undefined :: Bool) (undefined :: Int) >>= forceSum))
    , snapshot "views/dictionary-alias-identities"
        (dictionaryAliases . namesOf . callViews (undefined :: Bool) . safeCount)
    , snapshot "inferred/dictionary-alias-identities"
        (const (dictionaryAliases (namesOf (typedThunk (undefined :: Int) >>= inferredM))))
    , snapshot "distinct/four-binding-identities"
        (const (let names = namesOf (typedThunk (undefined :: Bool) >>= distinctM)
                in length (nub names) == 4))
    , snapshot "views/fresh-field-identities" (freshIdentities . safeCount)
    ]

safeCount :: Sample -> Int
safeCount = max 0 . number

typedThunk :: a -> M.MemoristM (M.Th a)
typedThunk = M.lazy . pure

callViews :: a -> Int -> M.MemoristM (M.PairT (M.PairT Int Int) (M.PairT Integer Integer))
callViews input count = do
    argument <- typedThunk input
    n <- typedThunk count
    viewsM argument n

observeViews :: a -> Int -> M.MemoristM Int
observeViews input count = callViews input count >>= forceSum

-- This caller must remain usable without Typeable or representation constraints.
generic :: Num a => M.Th a -> M.MemoristM Int
generic input = inferredM input >>= forceSum

freshViews :: Int -> Int -> M.MemoristM Int
freshViews repetitions count = do
    input <- typedThunk (undefined :: Bool)
    n <- typedThunk count
    sum <$> replicateM repetitions (viewsM input n >>= forceSum)

sourceSum :: ((Int, Int), (Integer, Integer)) -> Int
sourceSum ((a, b), (c, d)) = a + b + fromInteger (c + d)

sourceViews :: a -> Int -> Int
sourceViews input count = sourceSum (Source.views input count)

sourceInferred :: Num a => a -> Int
sourceInferred = sourceSum . Source.inferred

forceSum :: M.PairT (M.PairT Int Int) (M.PairT Integer Integer) -> M.MemoristM Int
forceSum (M.PairT ints integers) = do
    M.PairT first second <- M.force ints
    a <- M.force first
    b <- M.force second
    M.PairT third fourth <- M.force integers
    c <- M.force third
    d <- M.force fourth
    pure (a + b + fromInteger (c + d))

firstInt :: M.PairT (M.PairT Int Int) (M.PairT Integer Integer) -> M.MemoristM Int
firstInt (M.PairT ints _) = do
    M.PairT first _ <- M.force ints
    M.force first

secondInteger :: M.PairT (M.PairT Int Int) (M.PairT Integer Integer) -> M.MemoristM Int
secondInteger (M.PairT _ integers) = do
    M.PairT _ second <- M.force integers
    fromInteger <$> M.force second

projectViews :: Sample -> M.PairT (M.PairT Int Int) (M.PairT Integer Integer)
    -> (Maybe (Int, Int), Maybe (Integer, Integer))
projectViews sample (M.PairT ints integers) =
    (project (leftDemand sample) ints, project (rightDemand sample) integers)
  where
    project wanted root
        | not wanted = Nothing
        | otherwise = case M.tval root of
            M.PairT first second -> Just (M.tval first, M.tval second)

demandViews :: Sample -> M.PairT (M.PairT Int Int) (M.PairT Integer Integer)
    -> M.PairD (M.PairD Int Int) (M.PairD Integer Integer)
demandViews sample (M.PairT ints integers) = M.PairD
    (if leftDemand sample then M.fullDemand ints else M.Skip)
    (if rightDemand sample then M.fullDemand integers else M.Skip)

projectSingle :: Sample -> M.PairT (M.PairT Int Int) (M.PairT Integer Integer)
    -> (Maybe Int, Maybe Integer)
projectSingle sample (M.PairT ints integers) =
    (if leftDemand sample then Just (case M.tval ints of M.PairT first _ -> M.tval first) else Nothing,
     if rightDemand sample then Just (case M.tval integers of M.PairT _ second -> M.tval second) else Nothing)

demandSingle :: Sample -> M.PairT (M.PairT Int Int) (M.PairT Integer Integer)
    -> M.PairD (M.PairD Int Int) (M.PairD Integer Integer)
demandSingle sample (M.PairT ints integers) = M.PairD
    (if leftDemand sample then case M.tval ints of
        M.PairT first _ -> M.Need (M.PairD (M.fullDemand first) M.Skip)
     else M.Skip)
    (if rightDemand sample then case M.tval integers of
        M.PairT _ second -> M.Need (M.PairD M.Skip (M.fullDemand second))
     else M.Skip)

-- Only snapshots use computation runners; costed callbacks construct computations.
namesOf :: M.MemoristM (M.PairT (M.PairT Int Int) (M.PairT Integer Integer)) -> [M.ThName]
namesOf program = M.value (M.eval1 (program >>= fieldNames))

fieldNames :: M.PairT (M.PairT Int Int) (M.PairT Integer Integer) -> M.MemoristM [M.ThName]
fieldNames (M.PairT ints integers) = do
    M.PairT first second <- M.force ints
    M.PairT third fourth <- M.force integers
    pure [M.tname first, M.tname second, M.tname third, M.tname fourth]

dictionaryAliases :: [M.ThName] -> [Bool]
dictionaryAliases [a, b, c, d] =
    [a == b, c == d, all (`notElem` [c, d]) [a, b]]
dictionaryAliases _ = error "expected four scalar fields"

freshIdentities :: Int -> Bool
freshIdentities count = M.value $ M.eval1 $ do
    input <- typedThunk (undefined :: Bool)
    n <- typedThunk count
    first <- viewsM input n >>= fieldNames
    second <- viewsM input n >>= fieldNames
    pure (all (`notElem` second) first)
