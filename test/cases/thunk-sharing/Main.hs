module Main where
import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import qualified Memorist.Base.Thunk as MT
import Control.Monad (unless)
observeShared = do { bindings <- initialize; u <- M.lazy (pure ()); a <- sameM bindings u; b <- differentM bindings u; pure (a,b) }
observeLocal = do { bindings <- initialize; M.fromPure1 (localM bindings) (5::Int) }
demanded = do { (M.PairT a b, M.PairT c d) <- observeShared; a' <- M.force a; b' <- M.force b; c' <- M.force c; d' <- M.force d; pure (a'+b'+c'+fromInteger d') }
demandedLocal = do { M.PairT a b <- observeLocal; a' <- M.force a; b' <- M.force b; pure (a'+b') }
main = do
  unless (Source.same () == (10,10) && Source.different () == (10,10)) (fail "source oracle failed")
  let (M.PairT a b, M.PairT c d) = M.value (M.eval1 observeShared)
  unless (M.tname a == M.tname b && M.tname a == M.tname c) (fail "same dictionary specialization was re-thunked")
  unless (M.tname c /= M.tname d) (fail "different dictionary specializations share a name")
  let M.PairT l r = M.value (M.eval1 observeLocal)
  unless (M.tname l == M.tname r) (fail "local shared binding was re-thunked")
  unless (M.correct demanded (40::Int) && MT.toInteger (M.infcost demanded M.fullDemand) == 4) (fail "dictionary-sensitive sharing changed values or ticks")
  unless (M.correct demandedLocal (20::Int) && MT.toInteger (M.infcost demandedLocal M.fullDemand) == 2) (fail "local sharing changed values or ticks")
