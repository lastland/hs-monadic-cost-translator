module Main where
import Generated
import qualified Example as S
import Memorist.Memorist
import qualified Memorist.Base.Thunk as MT
import Test.QuickCheck
import Control.Monad (unless, replicateM_)
import System.Timeout (timeout)
program = fromPure1 growM (5::Int)
tree = value (eval1 program)
nodes LeafD = 1
nodes (NodeD a l r) = 1 + field a + branch l + branch r
field Skip = 0
field (Need _) = 1
branch Skip = 0
branch (Need d) = 1 + nodes d
compatible LeafT LeafD = True
compatible (NodeT a l r) (NodeD da dl dr) = scalar da && child l dl && child r dr
compatible _ _ = False
scalar Skip = True
scalar (Need n) = n == (5::Int)
child _ Skip = True
child t (Need d) = compatible (tval t) d
main = do
  unless (MT.toInteger (infcost' program (NodeD (Need 5) Skip Skip)) == 1) (fail "explicit finite demand cost")
  result <- timeout 10000000 $ replicateM_ 100 $ do
    d <- generate (genTreeDemand 15 tree)
    unless (nodes d <= 16) (fail "budget exceeded")
    unless (compatible tree d && all (compatible tree) (shrinkTreeDemand tree d)) (fail "incompatible demand/shrink")
    mapM_ (\s -> print (MT.toInteger (infcost' program s))) (shrinkTreeDemand tree d)
  case result of { Nothing -> fail "infinite demand generation"; Just () -> pure () }
  let nested = value (eval1 (reprM (S.More (S.End [1,2::Int])) :: MemoristM (NestedT Int)))
  d <- generate (genNestedDemand 10 nested)
  mapM_ print (shrinkNestedDemand nested d)
  custom <- generate (pure (NodeD Skip Skip Skip))
  unless (MT.toInteger (infcost' program custom) == 1) (fail "custom demand")
  putStrLn "PASS: datatype conversion, nested parameters, bounded compatible demands, shrinking and customization"
