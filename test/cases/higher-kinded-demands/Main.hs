{-# LANGUAGE DataKinds #-}
module Main where
import Generated
import qualified Example as S
import Memorist.Memorist
import Memorist.Data.List
import qualified Memorist.Base.Thunk as MT
import Test.QuickCheck
import Control.Monad (unless, replicateM_)
import System.Timeout (timeout)
program = fromPure1 forestM (5::Int)
tree = value (eval1 program)
nodes :: FixD ListD -> Integer
nodes (InD ds) = 1 + listNodes ds
listNodes Skip = 0
listNodes (Need NilD) = 0
listNodes (Need (ConsD h t)) = childNodes h + listNodes t
childNodes Skip = 0
childNodes (Need d) = nodes d
compatible (InT xs) (InD ds) = listCompatible (tval xs) ds
listCompatible _ Skip = True
listCompatible NilT (Need NilD) = True
listCompatible (ConsT h t) (Need (ConsD dh dt)) = childCompatible h dh && listCompatible (tval t) dt
listCompatible _ _ = False
childCompatible _ Skip = True
childCompatible t (Need d) = compatible (tval t) d
nestedNodes (NestD ds) = 1 + maybeNodes ds
maybeNodes Skip = 0
maybeNodes (Need NothingD) = 0
maybeNodes (Need (JustD ds)) = nestedList ds
nestedList Skip = 0
nestedList (Need NilD) = 0
nestedList (Need (ConsD h t)) = nestedChild h + nestedList t
nestedChild Skip = 0
nestedChild (Need d) = nestedNodes d
assert label ok = unless ok (fail label)
indexed = fromPure1 taggedM (5::Int) :: MemoristM (HigherT TaggedT 'S.Key)
main = do
  assert "literal index kinds value" (S.indexKinds 5 == 25 && correct (fromPure1 indexKindsM (5::Int)) (25::Int))
  assert "literal index kinds cost" (MT.toInteger (infcost (fromPure1 indexKindsM (5::Int)) fullDemand) == 6)
  let converted = value (eval1 (reprM (S.Higher (S.Layer (5::Int))) :: MemoristM (HigherT LayerT Int)))
  assert "custom constructor conversion" (case (abstract converted :: S.Higher S.Layer Int) of { S.Higher (S.Layer n) -> n == 5 })
  assert "index value" (case (abstract (value (eval1 indexed)) :: S.Higher S.Tagged 'S.Key) of { S.Higher (S.Tagged n) -> n == 5 })
  custom <- generate (genHigherDemand 12 converted)
  print custom
  let deepProgram = reprM (S.Higher (S.In [] :: S.Fix [])) :: MemoristM (HigherT FixT ListT)
  let deep = value (eval1 deepProgram)
  assert "constructor taking a constructor argument" (case (abstract deep :: S.Higher S.Fix []) of { S.Higher (S.In []) -> True; _ -> False })
  deepDemand <- generate (genHigherDemand 12 deep)
  print (shrinkHigherDemand deep deepDemand)
  assert "conversion dictionaries add no ticks" (MT.toInteger (infcost' deepProgram deepDemand) == 0)
  let finite = value (eval1 (reprM (S.Nest (Just []) :: S.Nest []) :: MemoristM (NestT ListT)))
  assert "nested recursive conversion" (case (abstract finite :: S.Nest []) of { S.Nest (Just []) -> True; _ -> False })
  let mixedProgram = reprM (S.Branch (Just (S.Branch (Just (S.Leaf [[5::Int]]))))) :: MemoristM (MixedT MaybeT Int)
  let mixed = value (eval1 mixedProgram)
  assert "non-regular higher-kinded conversion" (case (abstract mixed :: S.Mixed Maybe Int) of { S.Branch (Just (S.Branch (Just (S.Leaf n)))) -> n == [[5]]; _ -> False })
  mixedDemand <- generate (genMixedDemand 16 mixed)
  assert "non-regular higher-kinded demands and shrinks" (all (\d -> MT.toInteger (infcost' mixedProgram d) == 0) (mixedDemand : shrinkMixedDemand mixed mixedDemand))
  zero <- generate (genFixDemand 0 (InT (error "skipped field evaluated") :: FixT ListT))
  assert "zero budget" (zero == InD Skip)
  outcome <- timeout 10000000 $ replicateM_ 100 $ do
    budget <- generate (chooseInt (0,25))
    d <- generate (resize budget (arbDemandFrom tree))
    let shrunk = shrinkFixDemand tree d
    assert "compatible demand and shrinks" (compatible tree d && all (compatible tree) shrunk)
    assert "finite budget" (nodes d <= fromIntegral budget + 1)
    assert "one tick per observed producer node" (MT.toInteger (infcost' program d) == nodes d)
    assert "shrunk costs" (all (\s -> MT.toInteger (infcost' program s) == nodes s && nodes s <= nodes d) shrunk)
    indexDemand <- generate (genHigherDemand budget (value (eval1 indexed)))
    assert "non-Type index cost" (MT.toInteger (infcost' indexed indexDemand) == 1)
    let nestedProgram = fromPure1 nestedM (5::Int)
    nestedDemand <- generate (genNestDemand budget (value (eval1 nestedProgram)))
    assert "nested constructor application cost" (MT.toInteger (infcost' nestedProgram nestedDemand) == nestedNodes nestedDemand)
  case outcome of { Nothing -> fail "higher-kinded demand generation timed out"; Just () -> pure () }
