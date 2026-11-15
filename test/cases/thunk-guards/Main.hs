module Main where
import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import qualified Memorist.Base.Thunk as MT
import Control.Monad (unless)
check :: String -> (Int -> (Int,Int)) -> (M.Th Int -> M.MemoristM (M.PairT Int Int)) -> IO ()
check label source make = do
  unless (source 5 == (5,5) && case source undefined of { (_,_) -> True }) (fail (label ++ ": source oracle failed"))
  let program = do { x <- M.lazy (error "unused match subject evaluated" :: M.MemoristM Int); result <- make x; pure (M.tname x, result) }
      (result, next) = M.runM program M.nzero
      (name, M.PairT a b) = M.value result
  unless (M.tname a == name && M.tname b == name) (fail (label ++ ": match alias changed thunk identity"))
  unless (next == M.nnext M.nzero) (fail (label ++ ": match subject added a forwarding allocation"))
  unless (MT.toInteger (M.costAnnot result) == 1) (fail (label ++ ": match preparation changed entry ticks"))
  let demanded = do { x <- M.lazy (M.tick >> pure (5::Int)); M.PairT a b <- make x; av <- M.force a; bv <- M.force b; again <- M.force b; pure (av+bv+again) }
  unless (M.correct demanded (15::Int) && MT.toInteger (M.infcost demanded M.fullDemand) == 2) (fail (label ++ ": repeated demand changed values or sharing ticks"))
main = do
  check "guardAlias" Source.guardAlias guardAliasM
  check "guardParenthesized" Source.guardParenthesized guardParenthesizedM
  check "guardAnnotated" Source.guardAnnotated guardAnnotatedM
  check "guardAs" Source.guardAs guardAsM
