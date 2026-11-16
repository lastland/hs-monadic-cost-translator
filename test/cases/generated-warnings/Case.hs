module Case (probes) where

import Generated
import qualified Example as S
import qualified Memorist.Memorist as M
import Memorist.Data.List (ListT(..))
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "captured" (S.captured . number) (\s -> M.fromPure1 capturedM (number s))
    , scalarSource "choose" (S.choose . number) (\s -> M.fromPure1 chooseM (number s))
    , scalarSource "choose-zero" (\_ -> S.choose 0) (\_ -> M.fromPure1 chooseM (0 :: Int))
    , scalarSource "composed" (S.composed . other) (\s -> M.fromPure1 composedM (other s))
    , scalarSource "unbox" (\s -> S.unbox (S.Pair (number s) (other s)))
        (\s -> M.fromPure1 unboxM (S.Pair (number s) (other s)))
    , scalarSource "unbox-ignored-field" (\s -> S.unbox (S.Pair (other s) (error "unused record field")))
        (\s -> M.fromPure1 unboxM (S.Pair (other s) (error "unused record field")))
    , demanded "singleton" (\_ -> M.abstract :: ListT Int -> [Int]) (\_ -> M.fullDemand)
        (\s -> M.fromPure1 singletonM (number s))
    , scalarSource "captured-shared-input" (\s -> S.captured (S.captured (number s))) $ \s -> do
        input <- M.lazy (M.fromPure1 capturedM (number s))
        capturedM input
    , scalarSource "string-literal-pattern" (\s -> S.stringPattern (number s) (stringInput s)) $ \s -> do
        field <- M.lazy (pure (number s))
        text <- M.lazy (stringRepr (stringInput s))
        stringPatternM field text
    , failure "string-pattern-demanded-list" $ \s -> do
        field <- M.lazy (pure (number s))
        text <- M.lazy (error "undefined string")
        stringPatternM field text
    , scalarSource "string-pattern-unused-tail"
        (\s -> S.stringPattern (number s) ('b' : error "unused string tail")) $ \s -> do
            field <- M.lazy (pure (number s))
            text <- M.lazy (stringRepr ('b' : error "unused string tail"))
            stringPatternM field text
    , failure "string-pattern-demanded-tail" $ \s -> do
        field <- M.lazy (pure (number s))
        text <- M.lazy (stringRepr ('a' : error "demanded string tail"))
        stringPatternM field text
    ]

stringInput :: Sample -> String
stringInput s = case variant s `mod` 4 of
    0 -> ""
    1 -> "a"
    2 -> "b"
    _ -> "ab"

-- Char has no Repr instance in the runtime. Build the input's thunked spine
-- directly, without adding source-function ticks.
stringRepr :: String -> M.MemoristM (ListT Char)
stringRepr [] = pure NilT
stringRepr (x:xs) = ConsT <$> M.lazy (pure x) <*> M.lazy (stringRepr xs)
