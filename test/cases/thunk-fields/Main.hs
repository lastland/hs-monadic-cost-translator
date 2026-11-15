module Main where
import Generated
import qualified Memorist.Memorist as M
import qualified Memorist.Data.List as L
import qualified Memorist.Base.Thunk as MT
import Control.Monad (unless)
observe :: (M.Th Int -> M.MemoristM a) -> (M.ThName, a)
observe make = M.value (M.eval1 (do { x <- M.lazy (error "unused field evaluated" :: M.MemoristM Int); result <- make x; pure (M.tname x, result) }))
same label expected fields = unless (all ((== expected) . M.tname) fields) (fail (label ++ ": field thunk identity changed"))
checkBox label make = let (name, BoxT a b) = observe make in same label name [a,b]
main = do
  checkBox "constructor" constructorM
  checkBox "record" recordM
  let (pairName, M.PairT a b) = observe tupleM
  same "tuple" pairName [a,b]
  let (listName, L.ConsT h t) = observe listM
      L.ConsT h2 _ = M.tval t
  same "list" listName [h,h2]
  let consProgram = do { x <- M.lazy (error "unused head evaluated" :: M.MemoristM Int); xs <- M.lazy (error "unused tail evaluated" :: M.MemoristM (L.ListT Int)); result <- consM x xs; pure (M.tname x, M.tname xs, result) }
      (headName, tailName, L.ConsT ch ct) = M.value (M.eval1 consProgram)
  same "cons head" headName [ch]
  same "cons tail" tailName [ct]
  let (mixedName, BoxT reused computed) = observe mixedM
  same "mixed existing field" mixedName [reused]
  unless (M.tname computed /= mixedName) (fail "computed field did not receive a fresh thunk")
  unless (MT.toInteger (M.infcost' (M.fromPure1 mixedM (5::Int)) (BoxD M.Skip M.Skip)) == 1) (fail "computed field was evaluated before demand")
