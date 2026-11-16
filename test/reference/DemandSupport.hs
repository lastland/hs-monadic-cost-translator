{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
-- | Generated demands are choices made by the generated worker, not by the
-- reference. Capture those choices once and replay the exact finite plans in
-- both workers. Cases provide typed codecs and structural observations; this
-- module owns every assertion, timeout, and cost measurement.
module DemandSupport
    ( Plan(..), PlanCodec(..), DemandCodec(..), DemandProbe, DemandBatch(..), DemandObservation(..)
    , demandProbe, demandLabel, drawDemands, replayDemands
    , toDemandCodec, intPlan, thunkPlan, listPlan, maybePlan
    , treeBudget, constructorBudget
    ) where

import Control.Exception (evaluate)
import Control.Monad (unless, when)
import qualified Memorist.Memorist as M
import qualified Memorist.Base.Thunk as MT
import Memorist.Data.List (ListT(..), ListD(..))
import Protocol (Plan(..), DemandBatch(..), DemandObservation(..))
import System.Timeout (timeout)
import Test.QuickCheck (Gen, resize)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

-- Reusable structural codecs keep the case-specific adapters focused on the
-- source's own datatype constructors, including non-regular applications.
data PlanCodec a d = PlanCodec
    { toPlan :: d -> Plan
    , fromPlan :: Plan -> Maybe d
    , viewPlan :: a -> d -> Plan
    }

data DemandCodec a = DemandCodec
    { encodeDemand :: M.Dem a -> Plan
    , decodeDemand :: Plan -> Maybe (M.Dem a)
    -- Report actual constructors and scalar values at demanded positions.
    -- In particular, do not echo the requested constructor on a mismatch.
    , observeDemand :: a -> M.Dem a -> Plan
    , budgetSize :: Plan -> Int
    }

toDemandCodec :: PlanCodec a (M.Dem a) -> (Plan -> Int) -> DemandCodec a
toDemandCodec codec = DemandCodec (toPlan codec) (fromPlan codec) (viewPlan codec)

intPlan :: PlanCodec Int Int
intPlan = PlanCodec IntP decode (\actual _ -> IntP actual)
  where
    decode (IntP n) = Just n
    decode _ = Nothing

thunkPlan :: PlanCodec a d -> PlanCodec (M.Th a) (M.ThD d)
thunkPlan inner = PlanCodec encode decode observe
  where
    encode M.Skip = SkipP
    encode (M.Need d) = NeedP (toPlan inner d)
    decode SkipP = Just M.Skip
    decode (NeedP p) = M.Need <$> fromPlan inner p
    decode _ = Nothing
    observe _ M.Skip = SkipP
    observe t (M.Need d) = NeedP (viewPlan inner (M.tval t) d)

listPlan :: PlanCodec a d -> PlanCodec (ListT a) (ListD d)
listPlan element = codec
  where
    codec = PlanCodec encode decode observe
    headField = thunkPlan element
    tailField = thunkPlan codec
    encode NilD = ConP "Nil" []
    encode (ConsD a rest) = ConP "Cons" [toPlan headField a, toPlan tailField rest]
    decode (ConP "Nil" []) = Just NilD
    decode (ConP "Cons" [a, rest]) = ConsD <$> fromPlan headField a <*> fromPlan tailField rest
    decode _ = Nothing
    observe NilT _ = ConP "Nil" []
    observe (ConsT a rest) (ConsD da ds) = ConP "Cons" [viewPlan headField a da, viewPlan tailField rest ds]
    observe (ConsT _ _) _ = ConP "Cons" []

maybePlan :: PlanCodec a d -> PlanCodec (M.MaybeT a) (M.MaybeD d)
maybePlan element = PlanCodec encode decode observe
  where
    field = thunkPlan element
    encode M.NothingD = ConP "Nothing" []
    encode (M.JustD d) = ConP "Just" [toPlan field d]
    decode (ConP "Nothing" []) = Just M.NothingD
    decode (ConP "Just" [d]) = M.JustD <$> fromPlan field d
    decode _ = Nothing
    observe M.NothingT _ = ConP "Nothing" []
    observe (M.JustT a) (M.JustD d) = ConP "Just" [viewPlan field a d]
    observe (M.JustT _) _ = ConP "Just" []

data DemandProbe sample where
    DemandProbe :: M.Demand a =>
        String -> (sample -> M.MemoristM a) ->
        (Int -> a -> Gen (M.Dem a)) -> (a -> M.Dem a -> [M.Dem a]) ->
        DemandCodec a -> DemandProbe sample

demandProbe :: M.Demand a =>
    String -> (sample -> M.MemoristM a) ->
    (Int -> a -> Gen (M.Dem a)) -> (a -> M.Dem a -> [M.Dem a]) ->
    DemandCodec a -> DemandProbe sample
demandProbe = DemandProbe

demandLabel :: DemandProbe sample -> String
demandLabel (DemandProbe label _ _ _ _) = label

-- | Only the generated worker calls this operation. A seed makes failures
-- reproducible; correctness never relies on the reference using that seed.
drawDemands :: Int -> Int -> sample -> DemandProbe sample -> IO DemandBatch
drawDemands budget seed sample (DemandProbe label make generate shrink codec) = guarded label $ do
    unless (budget >= 0) (fail "negative demand budget")
    let result = M.eval1 (make sample)
        value = M.value result
        demand = unGen (resize budget (generate budget value)) (mkQCGen seed) budget
        smaller = shrink value demand
    -- This also forces the shrink-list spine under the shared timeout.
    n <- evaluate (length (take (planLimit + 1) smaller))
    when (n > planLimit) (fail "unbounded demand shrink list")
    let batch = DemandBatch label budget seed
            (encodeDemand codec demand) (map (encodeDemand codec) smaller)
    -- Check the original, typed generator results before serializing them.
    raw <- mapM (measureTyped codec result) (demand : smaller)
    checked <- checkBatch codec result batch
    unless (raw == checked) (fail "demand codec changes generated observations or costs")
    pure batch

-- | Neither replay path invokes a generator or a shrinker. The reference runs
-- its own source equations, under exactly the generated worker's demands.
replayDemands :: sample -> DemandBatch -> DemandProbe sample -> IO [DemandObservation]
replayDemands sample batch (DemandProbe label make _ _ codec) = guarded label $ do
    unless (batchLabel batch == label) (fail "demand batch belongs to a different probe")
    checked <- checkBatch codec (M.eval1 (make sample)) batch
    let prefix = label ++ "/budget-" ++ show (batchBudget batch) ++ "/seed-" ++ show (batchSeed batch)
        suffixes = "original" : ["shrink-" ++ show i | i <- [0 :: Int ..]]
    pure [DemandObservation (prefix ++ "/" ++ suffix) plan ticks |
        (suffix, (plan, ticks)) <- zip suffixes checked]

checkBatch :: M.Demand a => DemandCodec a -> M.Annotated a -> DemandBatch -> IO [(Plan, Integer)]
checkBatch codec result batch = do
    unless (batchBudget batch >= 0) (fail "negative replay budget")
    let plans = originalPlan batch : shrinkPlans batch
    n <- evaluate (length (take (planLimit + 1) plans))
    when (n > planLimit) (fail "unbounded replay batch")
    mapM_ finitePlan plans
    checked <- mapM check plans
    case checked of
        [] -> fail "missing original demand"
        (original, originalCost) : smaller -> mapM_ (checkShrink original originalCost) smaller
    pure checked
  where
    check plan = case decodeDemand codec plan of
        Nothing -> fail "demand plan has the wrong typed shape"
        Just demand -> do
            unless (encodeDemand codec demand == plan) (fail "demand codec does not round-trip")
            (actual, ticks) <- measureTyped codec result demand
            unless (actual == plan) (fail "demand is incompatible with the computed value")
            units <- evaluate (budgetSize codec plan)
            unless (units >= 0 && toInteger units <= toInteger (batchBudget batch) + 1)
                (fail "generated demand exceeds its node budget")
            pure (actual, ticks)
    checkShrink original originalCost (smaller, smallerCost) = do
        unless (smaller /= original && subplan smaller original) (fail "invalid demand shrink")
        unless (budgetSize codec smaller <= budgetSize codec original) (fail "demand shrink grows its node count")
        unless (smallerCost <= originalCost) (fail "demand shrink increases evaluation cost")

measureTyped :: M.Demand a => DemandCodec a -> M.Annotated a -> M.Dem a -> IO (Plan, Integer)
measureTyped codec result demand = do
    let actual = observeDemand codec (M.value result) demand
        ticks = MT.toInteger (M.calcCost result demand)
    finitePlan actual
    _ <- evaluate ticks
    pure (actual, ticks)

-- The limit guards faulty/infinite encoders and runaway generators. The
-- semantic budget is checked separately with the codec's structural measure.
planLimit :: Int
planLimit = 10000

finitePlan :: Plan -> IO ()
finitePlan plan = do
    remaining <- evaluate (walk planLimit plan)
    when (remaining < 0) (fail "demand plan is not finitely bounded")
  where
    walk fuel _ | fuel <= 0 = -1
    walk fuel SkipP = fuel - 1
    walk fuel (IntP n) = n `seq` fuel - 1
    walk fuel (NeedP p) = walk (fuel - 1) p
    walk fuel (ConP tag fields)
        | length (take 257 tag) > 256 = -1
        | otherwise = fieldsFuel (fuel - 1) fields
    fieldsFuel fuel _ | fuel < 0 = -1
    fieldsFuel fuel [] = fuel
    fieldsFuel fuel (p:ps) = fieldsFuel (walk fuel p) ps

subplan :: Plan -> Plan -> Bool
subplan SkipP _ = True
subplan (NeedP a) (NeedP b) = subplan a b
subplan (ConP a as) (ConP b bs) = a == b && length as == length bs && and (zipWith subplan as bs)
subplan (IntP a) (IntP b) = a == b
subplan _ _ = False

-- These measures are structural folds, not expected-cost formulas.
treeBudget :: Plan -> Int
treeBudget SkipP = 0
treeBudget (IntP _) = 0
treeBudget (NeedP p) = 1 + treeBudget p
treeBudget (ConP _ ps) = 1 + sum (map treeBudget ps)

constructorBudget :: String -> Plan -> Int
constructorBudget _ SkipP = 0
constructorBudget _ (IntP _) = 0
constructorBudget name (NeedP p) = constructorBudget name p
constructorBudget name (ConP tag ps) = (if name == tag then 1 else 0) + sum (map (constructorBudget name) ps)

guarded :: String -> IO a -> IO a
guarded label action = do
    result <- timeout 10000000 action
    case result of
        Nothing -> fail (label ++ ": demand generation/replay timed out")
        Just value -> pure value
