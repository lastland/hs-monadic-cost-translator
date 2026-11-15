module Main where
import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import qualified Memorist.Base.Thunk as MT
import Control.Monad (unless)
check :: String -> M.ThName -> (M.Th Int -> M.MemoristM Int) -> IO ()
check label expected forward = do
  let program = do { x <- M.lazy (error "unused argument evaluated" :: M.MemoristM Int); forward x }
      (result, next) = M.runM program M.nzero
  unless (M.value result == 42) (fail (label ++ ": forwarding changed the result"))
  unless (MT.toInteger (M.costAnnot result) == 2) (fail (label ++ ": forwarding changed entry ticks"))
  unless (next == expected) (fail (label ++ ": wrong thunk allocation: " ++ show next))
main = do
  unless (Source.g (undefined :: Int) == 42 && Source.computed (undefined :: Int) == 42) (fail "source oracle failed")
  let oneInput = M.nnext M.nzero
  check "plain" oneInput gM
  check "parenthesized" oneInput parenthesizedM
  check "annotated" oneInput annotatedM
  check "computed" (M.nnext oneInput) computedM
