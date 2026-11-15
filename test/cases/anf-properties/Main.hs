module Main where
import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import qualified Memorist.Data.List as L
import qualified Memorist.Base.Thunk as MT
import Test.QuickCheck
import Test.QuickCheck.Random (mkQCGen)
import Control.Monad (unless, replicateM)
type PairFn = M.Th Int -> M.MemoristM (M.PairT Int Int)
type ListFn = M.Th Int -> M.Th (L.ListT Int) -> M.MemoristM (L.ListT (M.PairT Int Int))
pairFixtures :: [(String, Int -> (Int,Int), PairFn, Int)]
pairFixtures = [("alias", Source.alias, aliasM, 0), ("separate", Source.separate, separateM, 2), ("local", Source.localPair, localPairM, 1), ("case-local", Source.caseLocal, caseLocalM, 1), ("case-annotated", Source.caseAnnotated, caseAnnotatedM, 1), ("guard-local", Source.guardLocal, guardLocalM, 1), ("guard-annotated", Source.guardAnnotated, guardAnnotatedM, 1), ("computed-case", Source.computedCase, computedCaseM, 1), ("computed-guard", Source.computedGuard, computedGuardM, 1)]
listFixtures :: [(String, Int -> [Int] -> [(Int,Int)], ListFn, Int)]
listFixtures = [("captured", Source.captured, capturedM, 0), ("computed-capture", Source.computedCapture, computedCaptureM, 1), ("specialized-composition", Source.specialized, specializedM, 1), ("nested-specialized-composition", Source.nestedSpecialized, nestedSpecializedM, 2)]
masks :: [(Bool,Bool)]
masks = [(False,False), (True,False), (False,True), (True,True)]
orderedMasks start = take 4 (drop start (cycle masks))
inputGen :: Gen (Int,Int,Int,Int)
inputGen = (,,,) <$> chooseInt (-40,40) <*> chooseInt (0,3) <*> chooseInt (1,8) <*> chooseInt (0,5)
shrinkInputs :: (Int,Int,Int,Int) -> [(Int,Int,Int,Int)]
shrinkInputs sample = filter bounded (shrink sample) where bounded (n,start,repeats,width) = abs n <= 40 && start >= 0 && start <= 3 && repeats >= 1 && repeats <= 8 && width >= 0 && width <= 5
cost :: M.MemoristM Int -> Integer
cost action = MT.toInteger (M.infcost action M.fullDemand)
observePure left right (a,b) = (if left then a else 0) + (if right then b else 0)
observePair :: Bool -> Bool -> M.PairT Int Int -> M.MemoristM Int
observePair left right (M.PairT a b) = do
  av <- if left then M.force a else pure 0
  bv <- if right then M.force b else pure 0
  pure (av+bv)
pairDemand :: Bool -> Bool -> (Int,Int) -> M.PairD Int Int
pairDemand left right (a,b) = M.PairD (if left then M.Need a else M.Skip) (if right then M.Need b else M.Skip)
checkPair :: Int -> Int -> (Bool,Bool) -> (String, Int -> (Int,Int), PairFn, Int) -> Property
checkPair n repeats (left,right) (label,source,make,workCopies) = counterexample (label ++ " / mask " ++ show (left,right)) $
  let (inputName,aName,bName) = M.value (M.eval1 (do { x <- M.lazy (pure n); M.PairT a b <- make x; pure (M.tname x,M.tname a,M.tname b) }))
      namesOK = if workCopies == 0 then aName == inputName && bName == inputName else aName /= inputName && bName /= inputName && ((aName == bName) == (workCopies == 1))
      observed = do { result <- M.fromPure1 make n; values <- replicateM repeats (observePair left right result); pure (sum values) }
      expected = repeats * observePure left right (source n)
      workEntries = if workCopies == 0 then 0 else if workCopies == 1 then fromEnum (left || right) else fromEnum left + fromEnum right
      expectedTicks = 1 + toInteger workEntries
  in conjoin [counterexample (show (inputName,aName,bName)) (namesOK === True), M.correct (M.fromPure1 make n) (source n) === True, M.correct observed expected === True, cost observed === expectedTicks, MT.toInteger (M.infcost' (M.fromPure1 make n) (pairDemand left right (source n))) === expectedTicks]
checkReturnedSharing :: Int -> Int -> (Bool,Bool) -> Property
checkReturnedSharing n repeats (left,right) = counterexample ("returned shared variable / mask " ++ show (left,right)) $
  let program = M.fromPure1 returnedSharedM n
      observed = do { result <- program; values <- replicateM repeats (observePair left right result); pure (sum values) }
      source = Source.returnedShared n
      expected = repeats * observePure left right source
      expectedTicks = 1 + toInteger (fromEnum left + fromEnum right + fromEnum (left || right))
  -- Each demanded keep call ticks, but both return the same shared work result.
  in conjoin [M.correct program source === True, M.correct observed expected === True, cost observed === expectedTicks, MT.toInteger (M.infcost' program (pairDemand left right source)) === expectedTicks]
inputNames L.NilT = []
inputNames (L.ConsT h t) = M.tname h : inputNames (M.tval t)
outputNames L.NilT = []
outputNames (L.ConsT h t) = let M.PairT a b = M.tval h in (M.tname a,M.tname b) : outputNames (M.tval t)
observeList :: Bool -> Bool -> L.ListT (M.PairT Int Int) -> M.MemoristM Int
observeList _ _ L.NilT = pure 0
observeList left right (L.ConsT h t) = do
  pair <- M.force h
  here <- observePair left right pair
  rest <- M.force t >>= observeList left right
  pure (here+rest)
listDemand :: Bool -> Bool -> [(Int,Int)] -> L.ListD (M.PairD Int Int)
listDemand left right = foldr (\pair rest -> L.ConsD (M.Need (pairDemand left right pair)) (M.Need rest)) L.NilD
checkList :: Int -> Int -> Int -> (Bool,Bool) -> (String, Int -> [Int] -> [(Int,Int)], ListFn, Int) -> Property
checkList n repeats width (left,right) (label,source,make,workEntries) = counterexample (label ++ " / mask " ++ show (left,right)) $
  let inputs = [n+1 .. n+width]
      (inputName,supplied,names) = M.value (M.eval1 (do { x <- M.lazy (pure n); xs <- M.reprM inputs; txs <- M.lazy (pure xs); output <- make x txs; pure (M.tname x,inputNames xs,outputNames output) }))
      captures = map fst names
      captureOK = case captures of { [] -> True; first:rest -> all (== first) rest && ((first == inputName) == (workEntries == 0)) }
      observed = do { result <- M.fromPure2 make n inputs; values <- replicateM repeats (observeList left right result); pure (sum values) }
      expected = repeats * sum (map (observePure left right) (source n inputs))
      expectedTicks = toInteger (2 * width + 2 + (if left && width > 0 then workEntries else 0))
  in conjoin [counterexample (show (inputName,supplied,names)) ((captureOK && length names == width && map snd names == supplied) === True), M.correct (M.fromPure2 make n inputs) (source n inputs) === True, M.correct observed expected === True, cost observed === expectedTicks, MT.toInteger (M.infcost' (M.fromPure2 make n inputs) (listDemand left right (source n inputs))) === expectedTicks]
-- Pair ticks: one entry, then one shared work entry or one per distinct demanded expression.
-- List ticks: one wrapper, width+1 map equations, width pair calls, and demanded shared work entries.
-- Driver traversal/repetition and callable adapters add no source ticks.
propertyFor = forAllShrink inputGen shrinkInputs $ \(n,start,repeats,width) -> conjoin
  [conjoin (checkReturnedSharing n repeats mask : map (checkPair n repeats mask) pairFixtures ++ map (checkList n repeats width mask) listFixtures) | mask <- orderedMasks start]
boundaries = conjoin ([checkList 7 4 width mask fixture | width <- [0,3], mask <- masks, fixture <- listFixtures] ++ [checkReturnedSharing 7 4 mask | mask <- masks])
checkProperty label args property = do
  result <- quickCheckWithResult args property
  unless (isSuccess result) (print result >> fail label)
main = do
  checkProperty "ANF empty/multi-output repeated-capture boundary" (stdArgs { maxSuccess = 1 }) boundaries
  checkProperty "ANF seeded sharing/value/demand/cost property" (stdArgs { maxSuccess = 100, replay = Just (mkQCGen 20261114,0) }) propertyFor
  checkProperty "ANF randomized sharing/value/demand/cost property" (stdArgs { maxSuccess = 100 }) propertyFor
