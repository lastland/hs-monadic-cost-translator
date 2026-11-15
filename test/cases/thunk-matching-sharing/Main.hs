module Main where
import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import qualified Memorist.Base.Thunk as MT
import Control.Monad (unless, replicateM, replicateM_)
assert label ok = unless ok (fail label)
sameNames [] = True
sameNames (name:rest) = all (== name) rest
checkScalar :: String -> M.MemoristM Int -> Int -> Integer -> IO ()
checkScalar label program expected ticks = assert label (M.correct program expected && MT.toInteger (M.infcost program M.fullDemand) == ticks)
topPoly = do { b <- initialize; u <- M.lazy (pure ()); a <- polyCaseM b u; c <- polyGuardM b u; pure (a,c) }
topPolyDemand = do { (M.PairT a b,M.PairT c d) <- topPoly; replicateM_ 6 (M.force a >> M.force b >> M.force c >> M.force d); pure (42::Int) }
localPoly = do { b <- initialize; u <- M.lazy (pure ()); a <- localPolyCaseM b u; c <- localPolyGuardM b u; pure (a,c) }
localPolyDemand = do { (M.PairT a b,M.PairT c d) <- localPoly; replicateM_ 6 (M.force a >> M.force b >> M.force c >> M.force d); pure (42::Int) }
numeric = do
  b <- initialize
  u <- M.lazy (pure ())
  x <- M.lazy (pure (0::Int))
  y <- M.lazy (pure (0::Integer))
  a <- sameCaseM b u
  c <- sameGuardM b u
  d <- differentM b u
  e <- scopedCaseM b x
  f <- scopedGuardM b y
  g <- localScopedM b x
  pure (a,c,d,e,f,g)
numericDemand = do
  (M.PairT a b,M.PairT c d,M.PairT e f,M.PairT g h,M.PairT i j,M.PairT k l) <- numeric
  totals <- replicateM 6 $ do
    av <- M.force a; bv <- M.force b; cv <- M.force c; dv <- M.force d; ev <- M.force e; fv <- M.force f
    gv <- M.force g; hv <- M.force h; iv <- M.force i; jv <- M.force j; kv <- M.force k; lv <- M.force l
    pure (sum [av,bv,cv,dv,ev,fromInteger fv,gv,hv,fromInteger iv,fromInteger jv,kv,lv])
  pure (sum totals)
wideProgram = do { b <- initialize; u <- M.lazy (pure ()); a <- wideBoolM b u; c <- wideIntM b u; d <- wideIntegerM b u; pure (a,c,d) }
wideDemand = do
  (M.PairT a b,M.PairT c d,M.PairT e f) <- wideProgram
  totals <- replicateM 6 $ do
    M.PairT av _ <- M.force a; M.PairT bv _ <- M.force b; M.PairT cv _ <- M.force c
    M.PairT dv _ <- M.force d; M.PairT ev _ <- M.force e; M.PairT fv _ <- M.force f
    av' <- M.force av; bv' <- M.force bv; cv' <- M.force cv; dv' <- M.force dv; ev' <- M.force ev; fv' <- M.force fv
    pure (sum [av',bv',cv',dv',fromInteger ev',fromInteger fv'])
  pure (sum totals)
defaultedDemand = do { b <- initialize; a <- M.fromPure1 (defaultedCaseM b) (42::Int); c <- M.fromPure1 (defaultedGuardM b) (42::Int); pure (a+c) }
checkBottom :: String -> (Int -> (Int,Int)) -> (Bindings -> M.Th Int -> M.MemoristM (M.PairT Int Int)) -> IO ()
checkBottom label source make = do
  assert (label ++ " source suspension") (fst (source 42) == 42)
  let program = do { b <- initialize; M.fromPure1 (make b) (42::Int) }
      M.PairT a _ = M.value (M.eval1 program)
  assert (label ++ " generated suspension/value/ticks") (M.tval a == 42 && MT.toInteger (M.infcost' program (M.PairD (M.Need 42) M.Skip)) == 1)
main = do
  let (ints@(M.PairT a b),bools@(M.PairT c d)) = M.value (M.eval1 topPoly)
  assert "top polymorphic matching identities across types" (sameNames [M.tname a,M.tname b,M.tname c,M.tname d])
  assert "top polymorphic source/generated values" ((M.abstract ints :: ([Int],[Int])) == Source.polyCase () && (M.abstract bools :: ([Bool],[Bool])) == Source.polyGuard ())
  checkScalar "top polymorphic matching: two entries plus one empty call" topPolyDemand 42 3
  let (localA@(M.PairT la lb),localB@(M.PairT lc ld)) = M.value (M.eval1 localPoly)
  assert "local polymorphic matching identities and evaluation freshness" (M.tname la == M.tname lb && M.tname lc == M.tname ld && M.tname la /= M.tname lc)
  assert "local polymorphic source/generated values" ((M.abstract localA :: ([Int],[Bool])) == Source.localPolyCase () && (M.abstract localB :: ([Int],[Bool])) == Source.localPolyGuard ())
  checkScalar "local polymorphic matching: two entries and two empty calls" localPolyDemand 42 4
  let (p@(M.PairT na nb),q@(M.PairT nc nd),r@(M.PairT ne nf),s@(M.PairT ng nh),t@(M.PairT ni nj),v@(M.PairT nk nl)) = M.value (M.eval1 numeric)
  assert "same dictionary names survive plain and scoped matching" (sameNames [M.tname na,M.tname nb,M.tname nc,M.tname nd,M.tname ne,M.tname ng,M.tname nh,M.tname nk,M.tname nl] && sameNames [M.tname nf,M.tname ni,M.tname nj] && M.tname na /= M.tname nf)
  assert "dictionary matching source/generated values" ((M.abstract p :: (Int,Int)) == Source.sameCase () && (M.abstract q :: (Int,Int)) == Source.sameGuard () && (M.abstract r :: (Int,Integer)) == Source.different () && (M.abstract s :: (Int,Int)) == Source.scopedCase (0::Int) && (M.abstract t :: (Integer,Integer)) == Source.scopedGuard (0::Integer) && (M.abstract v :: (Int,Int)) == Source.localScoped (0::Int))
  checkScalar "dictionary matching: seven source entries and two shared work calls" numericDemand 720 9
  let (M.PairT wa wb,M.PairT wc wd,M.PairT we wf) = M.value (M.eval1 wideProgram)
  assert "only dictionary-constrained parameters key matching identity" (sameNames [M.tname wa,M.tname wb,M.tname wc,M.tname wd] && M.tname we == M.tname wf && M.tname wa /= M.tname we)
  assert "wide matching source values" (Source.wideBool () == ((10,[]),(10,[])) && Source.wideInt () == ((10,[]),(10,[])) && Source.wideInteger () == ((10,[]),(10,[])))
  checkScalar "wide dictionary matching: three entries and two shared field computations" wideDemand 360 5
  assert "ignored concrete/scoped/defaulted source values" (Source.ignoredConcrete (42::Int) == 42 && Source.ignoredScoped (42::Int) == 42 && Source.ignoredDefault (42::Int) == 42 && Source.ignoredLocalScoped (42::Int) == 42)
  checkScalar "ignored concrete factory stays suspended" (do { b <- initialize; M.fromPure1 (ignoredConcreteM b) (42::Int) }) 42 1
  checkScalar "ignored scoped factory stays suspended" (do { b <- initialize; M.fromPure1 (ignoredScopedM b) (42::Int) }) 42 1
  checkScalar "ignored defaulted factory stays suspended" (do { b <- initialize; M.fromPure1 (ignoredDefaultM b) (42::Int) }) 42 1
  checkScalar "ignored locally scoped factory stays suspended" (do { b <- initialize; M.fromPure1 (ignoredLocalScopedM b) (42::Int) }) 42 2
  assert "demanded defaulted matching source values" (Source.defaultedCase (42::Int) + Source.defaultedGuard (42::Int) == 84)
  checkScalar "demanded defaulted matching shares one Integer specialization" defaultedDemand 84 3
  checkBottom "case local bottom" Source.bottomCase (\b x -> bottomCaseM b x)
  checkBottom "annotated guard local bottom" Source.bottomGuard (\b x -> bottomGuardM b x)
  checkBottom "computed case bottom" Source.computedBottomCase (\b x -> computedBottomCaseM b x)
  checkBottom "computed guard bottom" Source.computedBottomGuard (\b x -> computedBottomGuardM b x)
