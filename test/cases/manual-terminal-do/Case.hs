module Case (probes) where

import Control.Monad (replicateM, replicateM_)
import Data.List (elemIndex)
import Generated
import qualified Memorist.Base.Thunk as MT
import qualified Memorist.Memorist as M
import ReferenceSupport
import qualified TerminalDo as Source

type Choice = M.Th Bool -> M.Th Int -> M.Th Int -> M.MemoristM Int

probes :: [Probe]
probes = concatMap choiceChecks
    [("choose", Source.choose, chooseM), ("annotated", Source.annotated, annotatedM),
     ("computed", Source.computed, computedM), ("local", Source.local, localM)] ++
    [ scalarSource "work/value" (Source.work . number) (M.fromPure1 workM . number)
    , scalarSource "ignored-failure/true" (Source.ignoredFailure True . number)
        (M.fromPure2 ignoredFailureM True . number)
    , failure "ignored-failure/false" (M.fromPure2 ignoredFailureM False . number)
    , failure "bad/direct" (M.fromPure1 badM . number)
    ]

choiceChecks :: (String, Bool -> Int -> Int -> Int, Choice) -> [Probe]
choiceChecks (label, source, call) =
    [ scalarSource (label ++ "/value") (\s -> source (leftDemand s) (number s) (other s))
        (\s -> M.fromPure3 call (leftDemand s) (number s) (other s))
    , scalarSource (label ++ "/true-skips-bottom") (\s -> source True (number s) undefined)
        (\s -> M.fromPure3 call True (number s) (undefined :: Int))
    , scalarSource (label ++ "/false-skips-bottom") (source False undefined . other)
        (M.fromPure3 call False (undefined :: Int) . other)
    , scalarSource (label ++ "/costed-shared-inputs")
        (\s -> repeats s * source (leftDemand s) (number s) (other s)) $ \s -> do
        condition <- M.lazy (M.tick >> pure (leftDemand s))
        yes <- M.lazy (replicateM_ (width s + 1) M.tick >> pure (number s))
        no <- M.lazy (replicateM_ (width s + 2) M.tick >> pure (other s))
        sum <$> replicateM (repeats s) (call condition yes no)
    , snapshot (label ++ "/true-caller-allocation-and-entry-ticks") (const (resources call True))
    , snapshot (label ++ "/false-caller-allocation-and-entry-ticks") (const (resources call False))
    , failure (label ++ "/bottom-condition")
        (\s -> M.fromPure3 call (undefined :: Bool) (number s) (other s))
    , failure (label ++ "/selected-true-bottom")
        (M.fromPure3 call True (undefined :: Int) . other)
    , failure (label ++ "/selected-false-bottom")
        (\s -> M.fromPure3 call False (number s) (undefined :: Int))
    ]

resources :: Choice -> Bool -> (Int, Maybe Int, Integer)
resources call flag =
    let (result, next) = M.runM (M.fromPure3 call flag (5 :: Int) (9 :: Int)) M.nzero
    in (M.value result, elemIndex next (take 16 (iterate M.nnext M.nzero)),
        MT.toInteger (M.costAnnot result))
