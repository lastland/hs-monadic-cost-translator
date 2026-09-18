-- | Feasibility probes for the cost-translate generator conventions.
--
-- Every probe hand-translates a small source program exactly the way
-- @.scratch/translator-poc/CONVENTIONS.md@ says the generator will emit
-- it, then checks the cost that the MemoristANT runtime infers against an
-- independently derived expectation.  The arithmetic derivation of each
-- expected cost is written next to the probe.
--
-- Cost accounting recap (hs-pbt semantics):
--
--   * 'tick' adds 1 to the annotation of the current (innermost) level.
--   * @lazy m@ suspends @m@ in a named thunk; its cost is only paid when
--     the thunk is forced.
--   * Forcing a thunk adds @NameCost name (thunk cost)@ to the current
--     usage set.  The usage set is keyed by thunk /name/, so forcing the
--     SAME thunk twice still counts its cost exactly once (sharing).
--   * 'infcost' (via 'Memorist.Memorist.calcCost') returns
--     @(sum of usage-set entries) + (top-level annotation cost)@ under a
--     given demand on the result value.
module Probes where

import Control.Exception ( SomeException, evaluate, try )
import qualified Memorist.Example.List as ML
import Memorist.Data.List ( ListD (..), ListT (..) )
import qualified Memorist.Base.Thunk as MT
import Memorist.Memorist
import qualified Test.QuickCheck as QC

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Cost of @fM x@ under 'fullDemand', as an 'Integer'.
full1 :: (Memorist s m, Repr a, Demand b) => (Th (Rep a) -> m b) -> a -> Integer
full1 fM x = MT.toInteger (infcost (fromPure1 fM x) fullDemand)

full2
    :: (Memorist s m, Repr a, Repr b, Demand c)
    => (Th (Rep a) -> Th (Rep b) -> m c) -> a -> b -> Integer
full2 fM x y = MT.toInteger (infcost (fromPure2 fM x y) fullDemand)

full3
    :: (Memorist s m, Repr a, Repr b, Repr c, Demand d)
    => (Th (Rep a) -> Th (Rep b) -> Th (Rep c) -> m d) -> a -> b -> c -> Integer
full3 fM x y z = MT.toInteger (infcost (fromPure3 fM x y z) fullDemand)

-- | Cost of @fM x@ under an explicit demand (partial-demand checks).
part1
    :: (Memorist s m, Repr a, Demand b)
    => (Th (Rep a) -> m b) -> a -> Dem b -> Integer
part1 fM x d = MT.toInteger (infcost' (fromPure1 fM x) d)

len :: [a] -> Integer
len = fromIntegral . length

--------------------------------------------------------------------------------
-- P1: named callback (map succ)
--------------------------------------------------------------------------------

-- Source:
--
-- > mapSucc xs = map succ xs
--
-- Generator output (replacements map->ML.mapMM, succ->ML.succM; named
-- function reference in a callback position is suspended with
-- @tf <- lazy $ return succM@; the map call is a tail call):
p1MapSuccM :: Th (ListT Int) -> MemoristM (ListT Int)
p1MapSuccM txs = do
    tick
    tf <- lazy $ return ML.succM
    ML.mapMM tf txs

-- Cost derivation, input @[a1..an]@, 'fullDemand':
--
--   * 1   entry tick of mapSuccM
--   * 1   tick of the outermost ML.mapMM call (runs eagerly)
--   * n   ticks of succM, one per demanded element (each suspended in the
--         element thunk tz_i, forced by the demand)
--   * n   ticks of the recursive ML.mapMM calls, one per tail including
--         the final nil call (each suspended in tzs_i, forced by the
--         demand walking the spine)
--
--   total = 2n + 2.
p1Expected :: [Int] -> Integer
p1Expected xs = 2 * len xs + 2

p1Cost :: [Int] -> Integer
p1Cost = full1 p1MapSuccM

-- Partial demand @ConsD^k Skip@ (first k of n elements, n >= k):
--   * 1   entry tick
--   * 1   outermost mapMM tick
--   * k   succM ticks
--   * k-1 tail mapMM ticks (the k-th tail thunk is never demanded)
--
--   total = 2k + 1.  Here: n = 3, k = 2 => 5.
p1PartialCost :: Integer
p1PartialCost =
    part1 p1MapSuccM ([1, 2, 3] :: [Int])
        (ConsD (Need 0) (Need (ConsD (Need 0) Skip)))

p1PartialExpected :: Integer
p1PartialExpected = 5

p1Correct :: [Int] -> Bool
p1Correct xs = correct (fromPure1 p1MapSuccM xs) (map succ xs)

--------------------------------------------------------------------------------
-- P2: partial application at non-binary arity (add3)
--------------------------------------------------------------------------------

-- Source:
--
-- > add3 x y z = x + y + z
-- > sumWith n xs = foldr (add3 n) 0 xs    -- binary callback slot, 1 of 3 args
-- > mapAdd  n m xs = map (add3 n m) xs    -- unary callback slot,  2 of 3 args
--
-- Generator output: partial applications are allowed in callback
-- positions as @t <- lazy $ return (fM ta1..tak)@ (curryKM pattern; no
-- tick for the adapter itself).
add3M :: Th Int -> Th Int -> Th Int -> MemoristM Int
add3M tx ty tz = do
    tick
    x <- force tx
    y <- force ty
    z <- force tz
    return $ x + y + z

sumWithM :: Th Int -> Th (ListT Int) -> MemoristM Int
sumWithM tn txs = do
    tick
    tf <- lazy $ return $ add3M tn        -- supplied 1 of 3
    tz <- lazy $ return (0 :: Int)
    ML.foldrM tf tz txs

mapAddM :: Th Int -> Th Int -> Th (ListT Int) -> MemoristM (ListT Int)
mapAddM tn tm txs = do
    tick
    tf <- lazy $ return $ add3M tn tm     -- supplied 2 of 3
    ML.mapMM tf txs

-- Cost derivation for sumWithM on @[a1..ak]@, 'fullDemand' (the result is
-- an Int, so demand is trivially total):
--
--   * 1    entry tick of sumWithM
--   * k+1  ticks of ML.foldrM (one per cons, one for the nil)
--   * k    ticks of add3M (one per element, the accumulator chain forces
--          each suspended application exactly once)
--   * 0    forcing tf (return has no cost)
--
--   total = 2k + 2.
p2SumWithExpected :: [Int] -> Integer
p2SumWithExpected xs = 2 * len xs + 2

p2SumWithCost :: Int -> [Int] -> Integer
p2SumWithCost = full2 sumWithM

p2SumWithCorrect :: Int -> [Int] -> Bool
p2SumWithCorrect n xs =
    correct (fromPure2 sumWithM n xs) (foldr (\x acc -> x + acc + n) 0 xs)

-- Cost derivation for mapAddM on @[a1..ak]@, 'fullDemand':
--
--   * 1   entry tick
--   * k+1 mapMM ticks
--   * k   add3M ticks
--
--   total = 2k + 2.  Partial demand ConsD^k' Skip: 2k' + 1 as in P1.
p2MapAddExpected :: [Int] -> Integer
p2MapAddExpected xs = 2 * len xs + 2

p2MapAddCost :: Int -> Int -> [Int] -> Integer
p2MapAddCost = full3 mapAddM

p2MapAddCorrect :: Int -> Int -> [Int] -> Bool
p2MapAddCorrect n m xs =
    correct (fromPure3 mapAddM n m xs) (map (\x -> x + n + m) xs)

--------------------------------------------------------------------------------
-- P3: local function capturing a variable
--------------------------------------------------------------------------------

-- Source:
--
-- > example n xs = map (add n) xs
-- >   where add x y = x + y + n
--
-- Generator output: one local-function group (its own entry tick per
-- invocation), capturing @tn@ without re-forcing; the callback is the
-- partial application @addM tn@.
p3ExampleM :: Th Int -> Th (ListT Int) -> MemoristM (ListT Int)
p3ExampleM tn txs = do
    tick
    let addM tx ty = do
            tick
            x <- force tx
            y <- force ty
            n <- force tn
            return $ x + y + n
    tf <- lazy $ return (addM tn)
    ML.mapMM tf txs

-- Derivation, input length k, 'fullDemand':
--
--   * 1   entry tick of exampleM
--   * k+1 mapMM ticks
--   * k   addM ticks (one per demanded element)
--   * 0   forcing tn each time (the argument thunk itself costs 0 when it
--         comes from reprM; the sharing effect is measured in p3b below)
--
--   total = 2k + 2.
p3ExampleExpected :: [Int] -> Integer
p3ExampleExpected xs = 2 * len xs + 2

p3ExampleCost :: Int -> [Int] -> Integer
p3ExampleCost = full2 p3ExampleM

p3ExampleCorrect :: Int -> [Int] -> Bool
-- Source semantics: (add n) e = add n e = n + e + n, so each element
-- maps to e + 2n (the parameter x IS the passed n).
p3ExampleCorrect n xs =
    correct (fromPure2 p3ExampleM n xs) (map (\e -> e + n + n) xs)

-- An expensive helper used to measure sharing of the captured variable:
--
-- > expensive m = if m <= 0 then 0 else expensive (m - 1) + 1
expensiveM :: Th Int -> MemoristM Int
expensiveM tm = do
    tick
    m <- force tm
    if m <= 0 then return 0
    else do
        tm1 <- lazy $ return $ m - 1
        r <- expensiveM tm1
        return $ r + 1

-- @expensive 3@ costs 4 ticks (invocations on 3, 2, 1, 0).

-- Source:
--
-- > example3 xs = map (add n) xs
-- >   where add x y = x + y + n
-- >         n       = expensive 3
--
-- Generator output: the local VALUE bind becomes a thunk bind emitted
-- before the local-function group; every addM invocation forces the SAME
-- thunk @tn@.
p3Example3M :: Th (ListT Int) -> MemoristM (ListT Int)
p3Example3M txs = do
    tick
    t3 <- lazy $ return (3 :: Int)
    tn <- lazy $ expensiveM t3            -- local value bind, no tick
    let addM tx ty = do
            tick
            x <- force tx
            y <- force ty
            n <- force tn                 -- same thunk in every invocation
            return $ x + y + n
    tf <- lazy $ return (addM tn)
    ML.mapMM tf txs

-- Derivation, input length k, 'fullDemand':
--
--   * 1   entry tick
--   * k+1 mapMM ticks
--   * k   addM ticks (one per demanded element; note each addM
--         invocation forces the shared thunk tn twice, once as the
--         parameter x and once as the captured n — same name, so the
--         usage set still counts the forcing)
--   * 4   expensive (counted ONCE although forced inside every addM
--         invocation: all forces hit the same thunk name, and the usage
--         set is keyed by name) — but ONLY when k >= 1: with an empty
--         input list no addM ever runs, the expensive thunk is never
--         demanded, and laziness leaves its cost unpaid
--
--   total = 2k + 6  (k >= 1);  2  (k = 0).
p3Example3Expected :: [Int] -> Integer
p3Example3Expected xs
    | null xs   = 2
    | otherwise = 2 * len xs + 6

p3Example3Cost :: [Int] -> Integer
p3Example3Cost = full1 p3Example3M

p3Example3Correct :: [Int] -> Bool
-- (add n) e = n + e + n with n = expensive 3 = 3, so e maps to e + 6.
p3Example3Correct xs =
    correct (fromPure1 p3Example3M xs) (map (+ 6) xs)

-- Contrast (NOT a generator output): rebuild @n@ inside every callback
-- invocation.  Shows what the usage-set sharing saves.
p3Example3NSM :: Th (ListT Int) -> MemoristM (ListT Int)
p3Example3NSM txs = do
    tick
    let cb tx = do
            tick
            t3 <- lazy $ return (3 :: Int)
            tn <- lazy $ expensiveM t3
            x <- force tx
            n <- force tn
            return $ x + n
    tf <- lazy $ return cb
    ML.mapMM tf txs

--   * 1 + (k+1) + k * (1 cb + 4 expensive)  =  6k + 2.
p3Example3NSExpected :: [Int] -> Integer
p3Example3NSExpected xs = 6 * len xs + 2

p3Example3NSCost :: [Int] -> Integer
p3Example3NSCost = full1 p3Example3NSM

--------------------------------------------------------------------------------
-- P4: composition chains
--------------------------------------------------------------------------------

-- Source:  @mapSucc2 xs = map (succ . succ) xs@
--
-- Generator output: two-operand composition is
-- @tf <- lazy $ composeM succM succM@ (no tick for composition).
p4MapSucc2M :: Th (ListT Int) -> MemoristM (ListT Int)
p4MapSucc2M txs = do
    tick
    tf <- lazy $ composeM ML.succM ML.succM
    ML.mapMM tf txs

-- Derivation, input length k, 'fullDemand':
--
--   * 1   entry tick
--   * k+1 mapMM ticks
--   * 2k  succM ticks (inner + outer per element; composeM itself costs 0)
--
--   total = 3k + 2.  This is exactly the paper's mapSuccM2 parity target.
p4MapSucc2Expected :: [Int] -> Integer
p4MapSucc2Expected xs = 3 * len xs + 2

p4MapSucc2Cost :: [Int] -> Integer
p4MapSucc2Cost = full1 p4MapSucc2M

-- Parity with the paper's hand-written ML.mapSuccM2 under the same
-- randomly generated demands (compareOnSame derives the demand from the
-- first program's result and compares costs with (==)).
p4ParityMapSuccM2 :: [Int] -> QC.Property
p4ParityMapSuccM2 xs =
    compareOnSame (fromPure1 p4MapSucc2M xs) (fromPure1 ML.mapSuccM2 xs) (==)

-- Source:  @mapSucc3 xs = map (succ . succ . succ) xs@
--
-- Generator output for a 3-chain (CONVENTIONS):
-- @tf <- lazy (composeM e2 e3 >>= composeM e1)@.
p4MapSucc3M :: Th (ListT Int) -> MemoristM (ListT Int)
p4MapSucc3M txs = do
    tick
    tf <- lazy (composeM ML.succM ML.succM >>= composeM ML.succM)
    ML.mapMM tf txs

-- Derivation: entry 1 + mapMM (k+1) + 3k succM  =  4k + 2.
p4MapSucc3Expected :: [Int] -> Integer
p4MapSucc3Expected xs = 4 * len xs + 2

p4MapSucc3Cost :: [Int] -> Integer
p4MapSucc3Cost = full1 p4MapSucc3M

p4MapSucc2Correct :: [Int] -> Bool
p4MapSucc2Correct xs =
    correct (fromPure1 p4MapSucc2M xs) (map (+ 2) xs)

p4MapSucc3Correct :: [Int] -> Bool
p4MapSucc3Correct xs =
    correct (fromPure1 p4MapSucc3M xs) (map (+ 3) xs)

--------------------------------------------------------------------------------
-- P5: shared top-level value
--------------------------------------------------------------------------------

-- Source:
--
-- > numbers  = expensiveList 3
-- > answer x = x + head numbers + head numbers
--
-- with a spine-expensive list builder (tick per cons):
--
-- > expensiveList m = if m <= 0 then [] else m : expensiveList (m - 1)
expensiveListM :: Th Int -> MemoristM (ListT Int)
expensiveListM tm = do
    tick
    m <- force tm
    if m <= 0 then return NilT
    else do
        tm1 <- lazy $ return $ m - 1
        tr <- lazy $ expensiveListM tm1
        return $ ConsT tm tr

-- Generator output for the top-level value: an action (no tick), the rhs
-- suspended in ONE thunk.
p5NumbersM :: MemoristM (Th (ListT Int))
p5NumbersM = do
    t3 <- lazy $ return (3 :: Int)
    lazy $ expensiveListM t3

-- Generator output for answer: the referenced top-level value is hoisted
-- once at entry (after the tick) and the SAME thunk is used at both
-- occurrences; @head@ is a replacement (ML.headM) whose value-context
-- calls bind directly.
p5AnswerM :: Th Int -> MemoristM Int
p5AnswerM tx = do
    tick
    tnumbers <- p5NumbersM
    x <- force tx
    h1 <- ML.headM tnumbers
    h2 <- ML.headM tnumbers
    return $ x + h1 + h2

-- Derivation (result Int => full demand):
--
--   * 1   entry tick of answerM
--   * 2   headM ticks (two calls)
--   * 1   expensiveList paid ONCE: both headM calls force the SAME spine
--         thunk; head only demands the first cons, so only the first
--         expensiveList tick is inside that thunk (the tail is never
--         demanded), and the usage set counts that thunk once
--
--   total = 4.
p5AnswerCost :: Int -> Integer
p5AnswerCost = full1 p5AnswerM

p5AnswerExpected :: Integer
p5AnswerExpected = 4

p5AnswerCorrect :: Int -> Bool
p5AnswerCorrect x = correct (fromPure1 p5AnswerM x) (x + 3 + 3)

-- Non-shared variant (NOT a generator output): two separate hoistings.
p5AnswerNSM :: Th Int -> MemoristM Int
p5AnswerNSM tx = do
    tick
    x <- force tx
    h1 <- p5NumbersM >>= ML.headM
    h2 <- p5NumbersM >>= ML.headM
    return $ x + h1 + h2

--   * 1 + 2 headM + 2 expensiveList (two DISTINCT thunk names)  =  5.
p5AnswerNSCost :: Int -> Integer
p5AnswerNSCost = full1 p5AnswerNSM

p5AnswerNSExpected :: Integer
p5AnswerNSExpected = 5

-- Second shape: the expensive computation sits in the HEAD ELEMENT, so
-- the full 4-tick countdown is inside a single demanded thunk.
--
-- Source:
--
-- > countdown m = if m <= 0 then 0 else countdown (m - 1) + 1
-- > numbers2    = countdown 3 : []
-- > answer2 x   = x + head numbers2 + head numbers2
p5CountdownM :: Th Int -> MemoristM Int
p5CountdownM tm = do
    tick
    m <- force tm
    if m <= 0 then return 0
    else do
        tm1 <- lazy $ return $ m - 1
        r <- p5CountdownM tm1
        return $ r + 1

-- @countdown 3@ costs 4 ticks (invocations on 3, 2, 1, 0).

p5Numbers2M :: MemoristM (Th (ListT Int))
p5Numbers2M = do
    t3 <- lazy $ return (3 :: Int)
    tc <- lazy $ p5CountdownM t3
    tnil <- lazy $ return NilT
    lazy $ return (ConsT tc tnil)

p5Answer2M :: Th Int -> MemoristM Int
p5Answer2M tx = do
    tick
    tnumbers <- p5Numbers2M
    x <- force tx
    h1 <- ML.headM tnumbers
    h2 <- ML.headM tnumbers
    return $ x + h1 + h2

-- Derivation:
--
--   * 1   entry tick
--   * 2   headM ticks
--   * 4   countdown paid ONCE (the element thunk is shared between both
--         headM calls and its spine thunk costs 0)
--
--   total = 7.
p5Answer2Cost :: Int -> Integer
p5Answer2Cost = full1 p5Answer2M

p5Answer2Expected :: Integer
p5Answer2Expected = 7

p5Answer2Correct :: Int -> Bool
p5Answer2Correct x = correct (fromPure1 p5Answer2M x) (x + 3 + 3)

p5Answer2NSM :: Th Int -> MemoristM Int
p5Answer2NSM tx = do
    tick
    x <- force tx
    h1 <- p5Numbers2M >>= ML.headM
    h2 <- p5Numbers2M >>= ML.headM
    return $ x + h1 + h2

--   * 1 + 2 headM + 2 * 4 countdown (two distinct thunks)  =  11.
p5Answer2NSCost :: Int -> Integer
p5Answer2NSCost = full1 p5Answer2NSM

p5Answer2NSExpected :: Integer
p5Answer2NSExpected = 11

--------------------------------------------------------------------------------
-- P6: guard-order sensitivity (take variants)
--------------------------------------------------------------------------------

-- Source A (match BEFORE guard; GHC's historical/Report order):
--
-- > takeA n (x:xs) | n <= 0    = []
-- >                | otherwise = x : takeA (n - 1) xs
-- > takeA _ []     = []
--
-- Generator output: the list column contains only con patterns, so the
-- column thunk is forced BEFORE the guard of the ConsT branch is
-- evaluated.  @takeA 0 undefined@ therefore fails.
p6TakeAM :: Th Int -> Th (ListT Int) -> MemoristM (ListT Int)
p6TakeAM tn txs = do
    tick
    xs <- force txs                        -- column match precedes guard
    case xs of
        ConsT tx txs' -> do
            n <- force tn
            if n <= 0 then return NilT
            else do
                tn1 <- lazy $ return $ n - 1
                tzs <- lazy $ p6TakeAM tn1 txs'
                return $ ConsT tx tzs
        NilT -> return NilT

-- Source B (guard BEFORE match; the shape of ML.takeM):
--
-- > takeB n _  | n <= 0    = []
-- > takeB _ []             = []
-- > takeB n (x:xs)         = x : takeB (n - 1) xs
--
-- Generator output: the first row is all-var + guard, so the guard runs
-- before any column is forced.  @takeB 0 undefined@ returns [].
p6TakeBM :: Th Int -> Th (ListT Int) -> MemoristM (ListT Int)
p6TakeBM tn txs = do
    tick
    n <- force tn                          -- guard precedes match
    if 0 < n then do
        xs <- force txs
        case xs of
            ConsT tx txs' -> do
                tn1 <- lazy $ return $ n - 1
                tzs <- lazy $ p6TakeBM tn1 txs'
                return $ ConsT tx tzs
            NilT -> return NilT
    else return NilT

p6BottomSpine :: [Int]
p6BottomSpine = error "p6-bottom-spine"

-- (a) match-first on a bottom spine: demanded evaluation must FAIL.
p6TakeAOnBottom :: IO (Either SomeException Integer)
p6TakeAOnBottom =
    try (evaluate (full2 p6TakeAM (0 :: Int) p6BottomSpine))

-- (b) guard-first on a bottom spine: cost 1 (entry tick only), value [].
p6TakeBCostOnBottom :: IO (Either SomeException Integer)
p6TakeBCostOnBottom =
    try (evaluate (full2 p6TakeBM (0 :: Int) p6BottomSpine))

p6TakeBValueOnBottom :: IO (Either SomeException [Int])
p6TakeBValueOnBottom =
    try (evaluate
        (abstract (value (eval (fromPure2 p6TakeBM (0 :: Int) p6BottomSpine) initstate))))

-- Cost derivation on DEFINED input, take n xs with n <= length xs:
--
--   * n+1 entry ticks (invocations 0..n; the (n+1)-th sees the guard
--     n <= 0 or the NilT row and stops)
--
--   e.g. takeA/takeB 2 [1..5] = 3;  takeA/takeB 0 xs = 1.
p6TakeAOnDefined :: Int -> [Int] -> Integer
p6TakeAOnDefined = full2 p6TakeAM

p6TakeBOnDefined :: Int -> [Int] -> Integer
p6TakeBOnDefined = full2 p6TakeBM

p6DefinedExpected :: Int -> Integer
p6DefinedExpected n = fromIntegral (max 0 n) + 1

p6CorrectA :: Int -> [Int] -> Bool
p6CorrectA n xs = correct (fromPure2 p6TakeAM n xs) (take n xs)

p6CorrectB :: Int -> [Int] -> Bool
p6CorrectB n xs = correct (fromPure2 p6TakeBM n xs) (take n xs)

-- Both variants cost the same on defined inputs, under random demands.
p6ParityAB :: Int -> [Int] -> QC.Property
p6ParityAB n xs =
    compareOnSame (fromPure2 p6TakeAM n xs) (fromPure2 p6TakeBM n xs) (==)

-- Guard-first variant is cost-identical to the library takeM.
p6ParityBML :: Int -> [Int] -> QC.Property
p6ParityBML n xs =
    compareOnSame (fromPure2 p6TakeBM n xs) (fromPure2 ML.takeM n xs) (==)

-- Demand precision: defined first cons, bottom tail; taking 1 element
-- with the tail demand skipped never touches the bottom and costs 1
-- (a single takeA invocation: the suspended tail call is never forced).
p6ElemBottom :: [Int]
p6ElemBottom = 1 : error "p6-bottom-tail"

p6PartialDemandCost :: Integer
p6PartialDemandCost = MT.toInteger (infcost' m (ConsD (Need 0) Skip))
  where
    m :: MemoristM (ListT Int)
    m = do
        txs <- lazy $ reprM p6ElemBottom
        tn <- lazy $ return (1 :: Int)
        p6TakeAM tn txs

p6PartialDemandExpected :: Integer
p6PartialDemandExpected = 1

--------------------------------------------------------------------------------
-- P7: foldr with named append vs ML.concatM
--------------------------------------------------------------------------------

-- Source:
--
-- > myAppend []     ys = ys
-- > myAppend (x:xs) ys = x : myAppend xs ys
-- > myConcat xss = foldr myAppend [] xss
--
-- Generator output: @myAppend@ is a user-defined named function
-- translated normally; the callback position of the foldr replacement
-- suspends the name; [] becomes a NilT thunk; the foldr is a tail call.
p7MyAppendM :: Th (ListT Int) -> Th (ListT Int) -> MemoristM (ListT Int)
p7MyAppendM txs tys = do
    tick
    xs <- force txs
    case xs of
        NilT -> force tys
        ConsT tx txs' -> do
            tzs <- lazy $ p7MyAppendM txs' tys
            return $ ConsT tx tzs

p7MyConcatM :: Th (ListT (ListT Int)) -> MemoristM (ListT Int)
p7MyConcatM txss = do
    tick
    ta <- lazy $ return p7MyAppendM
    tz <- lazy $ return NilT
    ML.foldrM ta tz txss

-- Derivation, xss with K inner lists of total length L, 'fullDemand':
--
--   * 1      entry tick of myConcatM
--   * K+1    foldrM ticks
--   * L+K    myAppend ticks (append traverses inner list i in len_i + 1
--            invocations; summed over K inner lists)
--
--   total = L + 2K + 2.
p7Expected :: [[Int]] -> Integer
p7Expected xss = len (concat xss) + 2 * len xss + 2

p7Cost :: [[Int]] -> Integer
p7Cost = full1 p7MyConcatM

p7Correct :: [[Int]] -> Bool
p7Correct xss = correct (fromPure1 p7MyConcatM xss) (concat xss)

-- Parity with the library ML.concatM (identical translation shape)
-- under randomly generated demands.
p7ParityConcatM :: [[Int]] -> QC.Property
p7ParityConcatM xss =
    compareOnSame (fromPure1 p7MyConcatM xss) (fromPure1 ML.concatM xss) (==)
