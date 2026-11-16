module Case (probes) where

import Control.Monad (replicateM_)
import Generated
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport
import qualified ConstructorEquations as Source

type ListFunction = M.Th (L.ListT Int) -> M.MemoristM Int
type FlagFunction = M.Th Bool -> M.Th (L.ListT Int) -> M.MemoristM Int

probes :: [Probe]
probes = concatMap listChecks
    [("choose", Source.choose, chooseM), ("repeated", Source.repeated, repeatedM),
     ("head", Source.headValue, headValueM)] ++
    concatMap flagChecks [("wildcard", Source.wildcard, wildcardM), ("variable", Source.variable, variableM)] ++
    concatMap unaryGuardChecks
    [("guarded-wildcard", Source.guardedWildcard, guardedWildcardM),
     ("guarded-variable", Source.guardedVariable, guardedVariableM)] ++
    [ scalarSource "positive/value" (fromEnum . Source.positive . number)
        (\s -> fromEnum <$> M.fromPure1 positiveM (number s))
    , scalarSource "negative/value" (fromEnum . Source.negative . number)
        (\s -> fromEnum <$> M.fromPure1 negativeM (number s))
    , scalarSource "wildcard/guard-skips-bottom-spine" (const (Source.wildcard True undefined))
        (const (M.fromPure2 wildcardM True (undefined :: [Int])))
    , scalarSource "variable/nil-skips-bottom-guard" (const (Source.variable undefined []))
        (const (M.fromPure2 variableM (undefined :: Bool) ([] :: [Int])))
    , failure "wildcard/guard-before-nil" (const (M.fromPure2 wildcardM (undefined :: Bool) ([] :: [Int])))
    , failure "variable/spine-before-guard" (const (M.fromPure2 variableM True (undefined :: [Int])))
    , scalarSource "fallthrough/nil" (\s -> Source.fallthrough (number s) [])
        (\s -> M.fromPure2 fallthroughM (number s) ([] :: [Int]))
    , scalarSource "fallthrough/cons" (\s -> Source.fallthrough (other s) (number s : undefined)) $ \s -> do
        n <- M.lazy (M.tick >> pure (other s))
        xs <- partial s (number s)
        fallthroughM n xs
    , scalarSource "fallthrough/positive-skips-fallback"
        (\s -> Source.fallthrough undefined (abs (number s) + 1 : undefined))
        (\s -> M.fromPure2 fallthroughM (undefined :: Int) (abs (number s) + 1 : undefined))
    , failure "fallthrough/nil-demands-fallback"
        (const (M.fromPure2 fallthroughM (undefined :: Int) ([] :: [Int])))
    , failure "fallthrough/zero-demands-fallback"
        (const (M.fromPure2 fallthroughM (undefined :: Int) ((0 :: Int) : undefined)))
    , scalarSource "ordered/partial-tail" (Source.ordered . number)
        (M.fromPure1 orderedM . number)
    , failure "ordered/demanded-head" (const (M.fromPure1 orderedM (undefined :: Int)))
    ]

listChecks :: (String, [Int] -> Int, ListFunction) -> [Probe]
listChecks (label, source, call) =
    [ scalarSource (label ++ "/nil") (const (source [])) (const (M.fromPure1 call ([] :: [Int])))
    , scalarSource (label ++ "/finite") (source . numbers) (M.fromPure1 call . numbers)
    , scalarSource (label ++ "/sample-partial") (\s -> source (number s : undefined))
        (\s -> partial s (number s) >>= call)
    , scalarSource (label ++ "/positive-partial") (\s -> source (abs (number s) + 1 : undefined))
        (\s -> partial s (abs (number s) + 1) >>= call)
    , scalarSource (label ++ "/negative-partial") (\s -> source (negate (abs (number s) + 1) : undefined))
        (\s -> partial s (negate (abs (number s) + 1)) >>= call)
    , scalarSource (label ++ "/zero-partial") (const (source (0 : undefined)))
        (\s -> partial s 0 >>= call)
    , failure (label ++ "/bottom-spine") (const (M.fromPure1 call (undefined :: [Int])))
    , failure (label ++ "/bottom-head") (const (M.fromPure1 call ((undefined :: Int) : undefined)))
    ]

flagChecks :: (String, Bool -> [Int] -> Int, FlagFunction) -> [Probe]
flagChecks (label, source, call) =
    [ scalarSource (label ++ "/nil") (\s -> source (leftDemand s) [])
        (\s -> M.fromPure2 call (leftDemand s) ([] :: [Int]))
    , scalarSource (label ++ "/true-partial") (\s -> source True (number s : undefined))
        (\s -> M.fromPure2 call True (number s : undefined))
    , scalarSource (label ++ "/false-partial") (\s -> source False (number s : undefined))
        (\s -> M.fromPure2 call False (number s : undefined))
    , scalarSource (label ++ "/costed-input") (\s -> source (leftDemand s) (number s : undefined)) $ \s -> do
        flag <- M.lazy (M.tick >> pure (leftDemand s))
        xs <- partial s (number s)
        call flag xs
    ]

unaryGuardChecks :: (String, Bool -> [Int] -> Int, FlagFunction) -> [Probe]
unaryGuardChecks specimen@(label, source, call) = flagChecks specimen ++
    [ scalarSource (label ++ "/true-bottom-spine") (const (source True undefined))
        (const (M.fromPure2 call True (undefined :: [Int])))
    , scalarSource (label ++ "/true-nil") (const (source True []))
        (const (M.fromPure2 call True ([] :: [Int])))
    , scalarSource (label ++ "/false-nil") (const (source False []))
        (const (M.fromPure2 call False ([] :: [Int])))
    , scalarSource (label ++ "/false-costed-input") (\s -> source False (number s : undefined)) $ \s -> do
        flag <- M.lazy (M.tick >> pure False)
        xs <- partial s (number s)
        call flag xs
    , failure (label ++ "/false-bottom-spine")
        (const (M.fromPure2 call False (undefined :: [Int])))
    , failure (label ++ "/bottom-flag-before-nil")
        (const (M.fromPure2 call (undefined :: Bool) ([] :: [Int])))
    , failure (label ++ "/false-bottom-head")
        (const (M.fromPure2 call False ((undefined :: Int) : undefined)))
    ]

partial :: Sample -> Int -> M.MemoristM (M.Th (L.ListT Int))
partial s value = M.lazy $ do
    M.tick
    x <- M.lazy (replicateM_ (width s + 1) M.tick >> pure value)
    xs <- M.lazy (error "partial tail was inspected")
    pure (L.ConsT x xs)
