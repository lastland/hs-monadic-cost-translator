module Main where
import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import qualified Memorist.Base.Thunk as MT
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
assert label ok = unless ok (fail label)
pass label = putStrLn ("PASS: " ++ label)
checkInt :: String -> Int -> Integer -> Int -> M.MemoristM Int -> IO ()
checkInt label expected ticks source program = do
  assert (label ++ ": source value") (source == expected)
  assert (label ++ ": generated value") (M.correct program expected)
  assert (label ++ ": entry ticks") (MT.toInteger (M.infcost program M.fullDemand) == ticks)
  pass label
-- Only the simple scalar matchers below have this resource contract. fromPure1
-- allocates one outer input; reprM field allocations are enclosed in that thunk
-- and do not advance the caller's next name. These matchers add no caller thunks.
checkScalarMatch :: String -> Int -> Int -> M.MemoristM Int -> IO ()
checkScalarMatch label expected source program = do
  let (_,next) = M.runM program M.nzero
  assert (label ++ ": only outer input allocated at caller level") (next == M.nnext M.nzero)
  checkInt label expected 1 source program
checkBool :: String -> Bool -> Integer -> Bool -> M.MemoristM Bool -> IO ()
checkBool label expected ticks source program = do
  assert (label ++ ": source value") (source == expected)
  assert (label ++ ": generated value") (M.correct program expected)
  assert (label ++ ": entry ticks") (MT.toInteger (M.infcost program M.fullDemand) == ticks)
  pass label
fails :: String -> Bool -> Bool -> IO ()
fails label source generated = do
  a <- try (evaluate source) :: IO (Either SomeException Bool)
  b <- try (evaluate generated) :: IO (Either SomeException Bool)
  case (a,b) of { (Left _,Left _) -> pass label; _ -> fail (label ++ ": source and generated must both fail") }
fresh :: String -> (Int -> (Int,Int)) -> (M.Th Int -> M.MemoristM (M.PairT Int Int)) -> IO ()
fresh label source make = do
  assert (label ++ ": source spine") (case source undefined of { (_,_) -> True })
  let program = do { x <- M.lazy (error "unused input demanded" :: M.MemoristM Int); result <- make x; pure (M.tname x,result) }
      (result,next) = M.runM program M.nzero
      (name,M.PairT a b) = M.value result
  assert (label ++ ": fresh shared identity") (M.tname a /= name && M.tname a == M.tname b)
  -- nnext counts names only at this naming level, not allocations in suspended bodies.
  assert (label ++ ": one fresh subject or source anchor") (next == M.nnext (M.nnext M.nzero))
  assert (label ++ ": preparation ticks") (MT.toInteger (M.costAnnot result) == 1)
  assert (label ++ ": skipped fields") (MT.toInteger (M.infcost' (M.fromPure1 make (5::Int)) (M.PairD M.Skip M.Skip)) == 1)
  pass label
repeated :: String -> Int -> Integer -> (Int -> (Int,Int)) -> (M.Th Int -> M.MemoristM (M.PairT Int Int)) -> IO ()
repeated label expected ticks source make = do
  assert (label ++ ": source values") (source 5 == (expected,expected))
  let program = M.fromPure1 make (5::Int)
      demanded = do { M.PairT a b <- program; av <- M.force a; bv <- M.force b; again <- M.force a; pure (av+bv+again) }
  assert (label ++ ": generated values") (M.correct program (expected,expected) && M.correct demanded (3*expected))
  assert (label ++ ": one field cost") (MT.toInteger (M.infcost' program (M.PairD (M.Need expected) M.Skip)) == ticks)
  assert (label ++ ": both fields cost") (MT.toInteger (M.infcost' program (M.PairD (M.Need expected) (M.Need expected))) == ticks)
  assert (label ++ ": repeated force cost") (MT.toInteger (M.infcost demanded M.fullDemand) == ticks)
  pass label
asPattern :: String -> (Source.Box -> (Source.Box,Source.Box)) -> (M.Th BoxT -> M.MemoristM (M.PairT BoxT BoxT)) -> IO ()
asPattern label source make = do
  assert (label ++ ": source outer constructors") (case source (Source.Box undefined) of { (Source.Box _,Source.Box _) -> True })
  let program = do { field <- M.lazy (error "as-pattern field demanded" :: M.MemoristM Int); x <- M.lazy (pure (BoxT field)); result <- make x; pure (M.tname x,result) }
      (result,next) = M.runM program M.nzero
      (name,M.PairT a b) = M.value result
  assert (label ++ ": whole input identity") (M.tname a == name && M.tname b == name)
  assert (label ++ ": no forwarding allocation") (next == M.nnext (M.nnext M.nzero))
  let demand = M.PairD (M.Need (BoxD M.Skip)) (M.Need (BoxD M.Skip))
  assert (label ++ ": only outer structure demanded") (MT.toInteger (M.infcost' (M.fromPure1 make (Source.Box undefined)) demand) == 1)
  pass label
chooseProgram :: Bool -> Int -> Int -> M.MemoristM Int
chooseProgram c x y = do { tc <- M.lazy (pure c); tx <- M.lazy (pure x); ty <- M.lazy (pure y); chooseM tc tx ty }
main = do
  mapM_ (\(label,source,make) -> fresh label source make)
    [("computed case",Source.caseComputed,caseComputedM), ("computed guard",Source.guardComputed,guardComputedM),
     ("ignored conditional case",Source.caseConditional,caseConditionalM), ("ignored conditional guard",Source.guardConditional,guardConditionalM),
     ("ignored bad case",Source.caseBad,caseBadM), ("ignored bad guard",Source.guardBad,guardBadM),
     ("source-written let anchor",Source.sourceLet,sourceLetM)]
  -- Matching costs one entry; demanding work adds one, even across repeated forces.
  repeated "computed case demanded once" 10 2 Source.caseComputed caseComputedM
  repeated "computed guard demanded once" 10 2 Source.guardComputed guardComputedM
  repeated "source-written let reused" 5 1 Source.sourceLet sourceLetM
  mapM_ (\(label,source,make) -> checkScalarMatch label 7 (source undefined) (M.fromPure1 make (undefined::Int)))
    [("case variable",Source.caseVariable,caseVariableM), ("case wildcard",Source.caseWildcard,caseWildcardM),
     ("guard variable",Source.guardVariable,guardVariableM), ("guard wildcard",Source.guardWildcard,guardWildcardM)]
  mapM_ (\(label,source,make) -> do
    checkScalarMatch (label ++ " outer only") 7 (source (Source.Box undefined)) (M.fromPure1 make (Source.Box undefined))
    fails (label ++ " demands subject") (source undefined == 7) (M.correct (M.fromPure1 make (undefined::Source.Box)) (7::Int)))
    [("case constructor",Source.caseConstructor,caseConstructorM), ("guard constructor",Source.guardConstructor,guardConstructorM)]
  mapM_ (\(label,source,make) -> do
    checkScalarMatch (label ++ " match") 7 (source 0) (M.fromPure1 make (0::Int))
    checkScalarMatch (label ++ " fallback") 8 (source 1) (M.fromPure1 make (1::Int))
    fails (label ++ " demands subject") (source undefined == 7) (M.correct (M.fromPure1 make (undefined::Int)) (7::Int)))
    [("case literal",Source.caseLiteral,caseLiteralM), ("guard literal",Source.guardLiteral,guardLiteralM)]
  asPattern "case demanding as-pattern" Source.caseAs caseAsM
  asPattern "guard demanding as-pattern" Source.guardAs guardAsM
  mapM_ (\(label,source,make) -> do
    checkScalarMatch (label ++ " skips nested bottom") 2 (source (Source.Box 1,Source.Box undefined)) (M.fromPure1 make (Source.Box 1,Source.Box undefined))
    checkScalarMatch (label ++ " full match") 1 (source (Source.Box 0,Source.Box 0)) (M.fromPure1 make (Source.Box 0,Source.Box 0))
    fails (label ++ " reaches nested bottom") (source (Source.Box 0,Source.Box undefined) == 1) (M.correct (M.fromPure1 make (Source.Box 0,Source.Box undefined)) (1::Int)))
    [("case nested mismatch",Source.caseNested,caseNestedM), ("guard nested mismatch",Source.guardNested,guardNestedM)]
  mapM_ (\(label,source,make) -> do
    checkScalarMatch (label ++ " source order") 2 (source (Source.Record undefined 1)) (M.fromPure1 make (Source.Record undefined 1))
    checkScalarMatch (label ++ " full match") 1 (source (Source.Record 0 0)) (M.fromPure1 make (Source.Record 0 0))
    fails (label ++ " reaches later field") (source (Source.Record undefined 0) == 1) (M.correct (M.fromPure1 make (Source.Record undefined 0)) (1::Int)))
    [("case reverse record",Source.caseRecord,caseRecordM), ("guard reverse record",Source.guardRecord,guardRecordM)]
  mapM_ (\(label,source,make) -> do
    checkInt (label ++ " skips later guard") 2 1 (source (-1)) (M.fromPure1 make (-1::Int))
    fails (label ++ " reaches later guard") (source 1 == 1) (M.correct (M.fromPure1 make (1::Int)) (1::Int)))
    [("case guard sequence",Source.caseGuardSequence,caseGuardSequenceM), ("equation guard sequence",Source.guardSequence,guardSequenceM)]
  -- Ignoring the argument invokes only the wrapper and ignore, not the suspended body.
  checkInt "conditional argument suspended" 5 2 (Source.conditionalArg 5) (M.fromPure1 conditionalArgM (5::Int))
  checkInt "nested source-let argument suspended" 5 2 (Source.nestedLetArg 5) (M.fromPure1 nestedLetArgM (5::Int))
  -- Scalar return: wrapper + scalar + work. Primitive operands: wrapper + two work calls.
  checkInt "returned scalar demands work" 10 3 (Source.scalarWork 5) (M.fromPure1 scalarWorkM (5::Int))
  checkInt "primitive operands demand both work calls" 22 3 (Source.primitiveWork 5) (M.fromPure1 primitiveWorkM (5::Int))
  fails "returned scalar demands bottom" (Source.scalar undefined == 0) (M.correct (M.fromPure1 scalarM (undefined::Int)) (0::Int))
  fails "primitive left operand demanded" (Source.add undefined 5 == 0) (M.correct (M.fromPure2 addM (undefined::Int) (5::Int)) (0::Int))
  fails "primitive right operand demanded" (Source.add 5 undefined == 0) (M.correct (M.fromPure2 addM (5::Int) (undefined::Int)) (0::Int))
  checkInt "condition selects only true input" 5 1 (Source.choose True 5 undefined) (chooseProgram True 5 undefined)
  checkInt "condition selects only false input" 5 1 (Source.choose False undefined 5) (chooseProgram False undefined 5)
  -- conditionalWork + choose + positive + selected work; the bad branch stays suspended.
  checkInt "computed condition and selected input" 10 4 (Source.conditionalWork 5) (M.fromPure1 conditionalWorkM (5::Int))
  fails "condition itself demanded" (Source.choose undefined 5 5 == 5) (M.correct (chooseProgram undefined 5 5) (5::Int))
  fails "selected true input demanded" (Source.choose True undefined 5 == 5) (M.correct (chooseProgram True undefined 5) (5::Int))
  fails "selected false input demanded" (Source.choose False 5 undefined == 5) (M.correct (chooseProgram False 5 undefined) (5::Int))
  fails "selected bad computation demanded" (Source.conditionalWork (-5) == 0) (M.correct (M.fromPure1 conditionalWorkM (-5::Int)) (0::Int))
  checkBool "and skips right bottom" False 1 (Source.boolAnd False undefined) (M.fromPure2 boolAndM False (undefined::Bool))
  checkBool "or skips right bottom" True 1 (Source.boolOr True undefined) (M.fromPure2 boolOrM True (undefined::Bool))
  -- Short-circuit: wrapper + Boolean function + first positive; reaching the right adds positive + work.
  checkBool "and demands computed right" True 5 (Source.shortAnd 5) (M.fromPure1 shortAndM (5::Int))
  checkBool "and skips computed right" False 3 (Source.shortAnd (-5)) (M.fromPure1 shortAndM (-5::Int))
  checkBool "or skips computed right" True 3 (Source.shortOr 5) (M.fromPure1 shortOrM (5::Int))
  checkBool "or demands computed right" False 5 (Source.shortOr (-5)) (M.fromPure1 shortOrM (-5::Int))
  fails "and left input demanded" (Source.boolAnd undefined False == False) (M.correct (M.fromPure2 boolAndM (undefined::Bool) False) False)
  fails "and required right input demanded" (Source.boolAnd True undefined == False) (M.correct (M.fromPure2 boolAndM True (undefined::Bool)) False)
  fails "or left input demanded" (Source.boolOr undefined True == True) (M.correct (M.fromPure2 boolOrM (undefined::Bool) True) True)
  fails "or required right input demanded" (Source.boolOr False undefined == True) (M.correct (M.fromPure2 boolOrM False (undefined::Bool)) True)
