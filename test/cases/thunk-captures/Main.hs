module Main where
import Generated
import qualified Memorist.Memorist as M
import qualified Memorist.Data.List as L
import Control.Monad (unless)
observe :: (M.Th Int -> M.Th (L.ListT Int) -> M.MemoristM (L.ListT (M.PairT Int Int))) -> (M.ThName, L.ListT (M.PairT Int Int))
observe make = M.value (M.eval1 (do { x <- M.lazy (error "unused captured argument evaluated" :: M.MemoristM Int); nil <- M.lazy (pure L.NilT); xs <- M.lazy (pure (L.ConsT x nil)); result <- make x xs; pure (M.tname x, result) }))
check label make intermediate = do
  let (name, L.ConsT first _) = observe make
      M.PairT captured supplied = M.tval first
  unless (M.tname captured == name) (fail (label ++ ": captured argument was re-thunked"))
  unless ((M.tname supplied /= name) == intermediate) (fail (label ++ ": wrong supplied/intermediate thunk identity"))
main = do
  check "partial" partialM False
  check "composition" composedM True
