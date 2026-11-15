module Main (main) where

import Cost.Translate (translateFile)
import Control.Monad (unless, when)
import Control.Exception (SomeException, displayException, try)
import System.Directory (getTemporaryDirectory, doesFileExist, findExecutable)
import Data.List (isInfixOf, isPrefixOf, sort)
import Data.Char (isSpace)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import System.Process (readProcessWithExitCode)
import System.Environment (getArgs)

-- Deliberately retain fixtures and logs on failure for diagnosis.
main :: IO ()
main = do
    checkThunkForwarding
    checkThunkMatching
    checkAnfDemand
    checkThunkFields
    checkThunkCaptures
    checkThunkSharing
    checkThunkMatchingSharing
    checkAnfProperties
    let scopedAlias = unlines
          [ "{-# LANGUAGE ScopedTypeVariables #-}"
          , "module Example where"
          , "ignore :: t -> Int"
          , "ignore _ = 1"
          , "observed :: forall a. a -> Int"
          ]
    runAction "scoped-case-alias-shadow"
        (scopedAlias ++ "observed x = case (x :: a) of y -> let inner :: forall a. a -> Int; inner z = ignore y in inner True\n")
        "Source.observed (undefined :: Int)"
        "fromPure1 observedM (undefined :: Int)" 1 3
    runAction "scoped-guard-alias-shadow"
        (scopedAlias ++ "observed x | y <- (x :: a) = let inner :: forall a. a -> Int; inner z = ignore y in inner True\n")
        "Source.observed (undefined :: Int)"
        "fromPure1 observedM (undefined :: Int)" 1 3
    let scopedFactory = unlines
          [ "{-# LANGUAGE ScopedTypeVariables #-}"
          , "module Example where"
          , "work n | n == 0 = n+1 | otherwise = work (n-n)+1"
          , "shared :: (Num a, Eq a) => a"
          , "shared = work 18446744073709551616"
          , "observed :: forall a. (Num a, Eq a) => a -> Int"
          ]
    mapM_ (\(label, body) -> do
        runAction (label ++ "-int") (scopedFactory ++ body)
            "Source.observed (0::Int)"
            "do { b <- initialize; fromPure1 (observedM b) (0::Int) }" 1 3
        runAction (label ++ "-integer") (scopedFactory ++ body)
            "Source.observed (0::Integer)"
            "do { b <- initialize; fromPure1 (observedM b) (0::Integer) }" 2 4)
        [ ("scoped-factory-case-shadow", "observed x = case (shared :: a) of y -> let inner :: forall a. a -> Int; inner z = if y == 1 then 1 else 2 in inner True\n")
        , ("scoped-factory-guard-shadow", "observed x | y <- (shared :: a) = let inner :: forall a. a -> Int; inner z = if y == 1 then 1 else 2 in inner True\n")
        ]
    runExample "double"
        "double :: Int -> Int\ndouble x = x + x\n"
        "doubleM" 10 1
    runExample "shared-local"
        "double x = x + x\nshared x = let y = double x in y + y\n"
        "sharedM" 20 2
    runExample "shared-forwarded-arguments"
        "work x = x + x\nadd a b = a + b\nshared x = let y = work x in add y y\n"
        "sharedM" 20 3
    runExample "unused-failing-control-flow"
        "keep x = let unused = if (undefined :: Bool) then x else x in x\n"
        "keepM" 5 1
    runExample "captured-local-function"
        "double x = x + x\ncaptured x = let y = double x; add z = y + z in add x + add x\n"
        "capturedM" 30 4
    runExample "readable-name-argument-collision"
        "third _ _ z = z\nobserved arg0' = third 1 2 arg0'\n"
        "observedM" 5 2
    runExample "readable-name-global-collision"
        "double x = x + x\nobserved x = let doubleM y = y + 1 in double (doubleM x)\n"
        "observedM" 12 3
    runExample "readable-name-field-collision"
        "pick field0 ((x,_),_) = field0 + x\nobserved n = pick n ((3,0),0)\n"
        "observedM" 8 2
    runExample "readable-name-anchor-collision"
        "observed xanchor = let x = xanchor + 1 in x\n"
        "observedM" 6 1
    runExample "readable-name-case-thunk-collision"
        "observed x = let caseThunk0 y = y + 1 in case x of z -> caseThunk0 z\n"
        "observedM" 6 2
    runExample "readable-name-continuation-collisions"
        "choose 0 = 0\nchoose nextEquation0 = nextEquation0\nobserved nextGuard0 | nextGuard0 > 0 = choose nextGuard0 | otherwise = 0\n"
        "observedM" 5 2
    runExample "readable-name-guard-thunk-collision"
        "observed guardThunk0 | value <- guardThunk0 = value\n"
        "observedM" 5 1
    runExample "readable-parenthesized-parameter"
        "observed (source) = source\n"
        "observedM" 5 1
    checkWholeModuleFailure
    runExample "handwritten-filter-lazy-tail"
        "keep x = True\nfiltered x = head (filter keep (x : undefined))\n"
        "filteredM" 5 4
    runExample "ordered-equations-and-conjunctive-guards"
        (unlines ["choose xs | False = undefined", "choose [] = undefined",
                  "choose (y:_) | True, False = undefined | True = y",
                  "ordered x = choose (x : undefined)"])
        "orderedM" 5 2
    runExample "finite-observation-of-recursive-producer"
        "stream n = n : stream (n + 1)\nsecond (_:x:_) = x\nobserve n = second (stream n)\n"
        "observeM" 6 4
    runExample "ternary-partials-capture-and-composition"
        (unlines ["add3 a b c = a + b + c", "twice x = x + x",
          "partial x = let captured z = x + z in head (map (captured . add3 x x . twice) (x : undefined))"])
        "partialM" 25 6
    runAction "shared-top-level-across-exported-calls"
        "seed :: Int\nseed = double 5\ndouble x = x + x\nfirst x = seed + x\nsecond x = seed + x\n"
        "Source.first (5::Int) + Source.second 5"
        "do { bindings <- initialize; a <- fromPure1 (firstM bindings) (5::Int); b <- fromPure1 (secondM bindings) (5::Int); pure (a+b) }"
        30 3
    runExample "scanl-unused-input"
        "add a b = a + b\nbad n = if (undefined :: Bool) then [n] else []\nscanned x = head (scanl add x (bad x))\n"
        "scannedM" 5 3
    runExample "lazy-datatype-primitive-field"
        "data Box a = Box a Int\nwrap x = Box x undefined\nunbox (Box x _) = x\nboxed x = unbox (wrap x)\n"
        "boxedM" 5 3
    checkDemands
    runExample "foldr-arity-four-partial"
        "add4 a b c d = a + b + c + d\nfolded x = foldr (add4 x x) 0 [x,x]\n"
        "foldedM" 30 6
    runExample "foldl-named"
        "add a b = a + b\nfolded x = foldl add 0 [x,x]\n"
        "foldedM" 10 6
    runExample "zipWith-named"
        "add a b = a + b\nzipped x = head (zipWith add (x:undefined) (x:undefined))\n"
        "zippedM" 10 4
    runExample "concatMap-named"
        "singleton x = [x]\nflattened x = head (concatMap singleton (x:undefined))\n"
        "flattenedM" 5 6
    runExample "repeatly-external-identity"
        "import Memorist.Example.NewtonRaphson (repeatly)\nstep x = x + x\nrepeated x = head (tail (repeatly step x))\n"
        "repeatedM" 10 4
    runExample "mutually-recursive-captured-helpers"
        "mutual x = let evenN n | n == 0 = x | otherwise = oddN (n-1); oddN n | n == 0 = 0 | otherwise = evenN (n-1) in evenN 4\n"
        "mutualM" 5 6
    runExample "case-pattern-and-let-guards"
        "guarded x = case x:undefined of { ys | h:_ <- ys, let z = h+h, z > 0 && True -> z; _ -> undefined }\n"
        "guardedM" 10 1
    runExample "literal-pattern-fallback"
        "choose 0 = undefined\nchoose n = n\nliteral x = choose x\n"
        "literalM" 5 2
    runExample "char-scalar-field"
        "data Box = Box Char\nreadBox (Box c) = 5\nobserved x = readBox (Box 'a')\n"
        "observedM" 5 2
    runExample "literal-pattern-char-string"
        (unlines ["tag 'a' = 1", "tag _ = 2", "named \"ab\" = 3", "named _ = 4",
                  "pick c = if c == 'z' then 10 else 20",
                  "observed x = tag 'a' + named ('a':'b':[]) + pick 'z'"])
        "observedM" 14 4
    runExample "direct-composition-application"
        "add3 a b c = a+b+c\ndouble x = x+x\ncomposed x = (add3 x . double) x x\n"
        "composedM" 20 3
    runAction "polymorphic-shared-top-level-value"
        "empty n | n > 0 = [] | otherwise = []\nshared = empty (5::Int)\nints x = case shared of { [] -> x; _ -> x }\nbools x = case shared of { [] -> x; _ -> x }\n"
        "Source.ints (5::Int)"
        "do { bindings <- initialize; b <- fromPure1 (boolsM bindings) True; if b then fromPure1 (intsM bindings) (5::Int) else pure 0 }"
        5 3
    runExample "polymorphic-local-value"
        "empty n = []\nints :: [Int] -> Int\nints [] = 5\nbools :: [Bool] -> Int\nbools [] = 5\nlocal x = let shared = empty x in ints shared + bools shared\n"
        "localM" 10 4
    runExample "resolved-user-map"
        "import Prelude hiding (map)\nmap x = x+x\nwrapped x = map x\n"
        "wrappedM" 10 2
    runExample "unused-error-and-short-circuit"
        "keep x = let unused = error \"unused\" in if True || undefined then x else unused\n"
        "keepM" 5 1
    runExample "record-lazy-fields-and-selectors"
        "data Box = Box { payload :: Int, ignored :: Int }\nreadBox Box { payload = x } = x\nrecorded x = let b = Box { ignored = undefined, payload = x } in readBox b + payload b\n"
        "recordedM" 10 3
    runExample "nested-pair-and-maybe-fields"
        "data Nested a = End a | More (Nested (a,a))\nreadNested (More (End (x,_))) = x\nreadMaybe (Just x) = x\nnested x = readMaybe (Just (readNested (More (End (x,undefined)))))\n"
        "nestedM" 5 3
    runExample "nested-case-retains-outer-binding"
        "nested x = case x of { y -> case x+x of { z -> y+z } }\n"
        "nestedM" 15 1
    runExample "named-source-operator"
        "a %% b = a+b\noperated x = x %% x\n"
        "operatedM" 10 2
    runExample "infix-constructor"
        "data Pair a = a :*: a\nfirst (x :*: _) = x\npaired x = first (x :*: undefined)\n"
        "pairedM" 5 2
    checkDiagnostics
    checkRuntimeFailures
    checkGeneratedProperties
    runAction "direct-foldr-maybe"
        "difference x y = x-y\nfolded xs = foldr difference 10 xs\n"
        "Source.folded (Just (5::Int))"
        "fromPure1 foldedM (Just (5::Int))" (-5) 3
    runAction "direct-foldl-pair-unused-first-field"
        "difference x y = x-y\nfolded xs = foldl difference 10 xs\n"
        "Source.folded (undefined :: Bool, 5::Int)"
        "fromPure1 foldedM (undefined :: Bool, 5::Int)" 5 3
    runAction "direct-foldr-nothing-unused-callback"
        "add x y = undefined\nfolded xs = foldr add (5::Int) xs\n"
        "Source.folded (Nothing :: Maybe Int)"
        "fromPure1 foldedM (Nothing :: Maybe Int)" 5 2
    checkPatternScaling
    runExample "scalar-division-and-negation"
        "arithmetic x = ((x*3) `div` 2) + (-1)\n"
        "arithmeticM" 6 1
    runAction "direct-concatMap-maybe"
        "single x = [x]\nflat xs = concatMap single xs\nobserved xs = head (flat xs)\n"
        "Source.observed (Just (5::Int))"
        "fromPure1 observedM (Just (5::Int))" 5 7
    runExample "explicit-constructor-exports"
        "module Example (Box(..), make, readBox, observed) where\ndata Box = Box Int\nmake x = Box x\nreadBox (Box x) = x\nobserved x = readBox (make x)\n"
        "observedM" 5 3
    checkHigherKindedAudit
    checkHigherKindedDemands
    runExample "datatype-name-shadows-foldable-class"
        "import Prelude hiding (Foldable)\ndata Foldable a = Box a\nunbox (Box x) = x\nobserved x = unbox (Box x)\n"
        "observedM" 5 2
    let constrained = unlines
          ["expensive n | n == 0 = n+1 | otherwise = expensive (n-n)+1",
           "shared :: (Eq a, Num a) => a", "shared = expensive 18446744073709551616",
           "asInt :: Int -> Int", "asInt x = shared+x",
           "asInteger :: Integer -> Integer", "asInteger x = shared+x"]
    runAction "constrained-sharing-different-dictionaries" constrained
        "if (Source.asInt 0, Source.asInteger 0) == (1,2) then 12 else 0"
        "do { b <- initialize; i <- fromPure1 (asIntM b) (0::Int); j <- fromPure1 (asIntegerM b) (0::Integer); pure (if (i,j) == (1,2) then 12 else 0) }"
        12 5
    runAction "constrained-sharing-same-dictionary" constrained
        "Source.asInt 0 + Source.asInt 0"
        "do { b <- initialize; i <- fromPure1 (asIntM b) (0::Int); j <- fromPure1 (asIntM b) (0::Int); pure (i+j) }"
        2 3
    runAction "constrained-sharing-unconstrained-parameter"
        (unlines ["expensive n | n == 0 = n+1 | otherwise = expensive (n-n)+1",
                  "shared :: (Eq a, Num a) => (a, [b])", "shared = (expensive 18446744073709551616, [])",
                  "one x = case (shared :: (Int,[Bool])) of { (n,_) -> n+x }",
                  "two x = case (shared :: (Int,[Char])) of { (n,_) -> n+x }"])
        "Source.one 0 + Source.two 0"
        "do { b <- initialize; i <- fromPure1 (oneM b) (0::Int); j <- fromPure1 (twoM b) (0::Int); pure (i+j) }"
        2 3
    runExample "constrained-sharing-local"
        (unlines ["expensive n | n == 0 = n+1 | otherwise = expensive (n-n)+1",
                  "local x = let shared :: (Eq a, Num a) => a; shared = expensive 18446744073709551616; one n = (shared :: Int)+n; two n = (shared :: Integer)+n in if one 0 == 1 && two 0 == 2 then x else 0"])
        "localM" 5 6
    runAction "constrained-sharing-generic-caller"
        (unlines ["expensive n | n == 0 = n+1 | otherwise = expensive (n-n)+1",
                  "shared :: (Eq a, Num a) => a", "shared = expensive 18446744073709551616",
                  "generic x = shared+x", "asInt :: Int -> Int", "asInt x = generic x",
                  "asInteger :: Integer -> Integer", "asInteger x = generic x"])
        "if (Source.asInt 0, Source.asInteger 0) == (1,2) then 12 else 0"
        "do { b <- initialize; i <- fromPure1 (asIntM b) (0::Int); j <- fromPure1 (asIntegerM b) (0::Integer); pure (if (i,j) == (1,2) then 12 else 0) }"
        12 7
    runAction "constrained-sharing-ignored-signature"
        "costly n = n+n\nshared :: Num a => a\nshared = costly 5\nignore unused x = x\nobserved x = ignore (shared :: Int) x\n"
        "Source.observed (5::Int)"
        "do { b <- initialize; fromPure1 (observedM b) (5::Int) }" 5 2
    runAction "constrained-sharing-scoped-signature"
        "{-# LANGUAGE ScopedTypeVariables #-}\nmodule Example where\ncostly n = n+n\nshared :: Num a => a\nshared = costly 5\nignore unused x = x\nobserved :: forall a. Num a => a -> a\nobserved x = ignore ((shared+shared) :: a) x\n"
        "Source.observed (5::Int)"
        "do { b <- initialize; fromPure1 (observedM b) (5::Int) }" 5 2
    runAction "constrained-sharing-local-scoped-signature"
        "{-# LANGUAGE ScopedTypeVariables #-}\nmodule Example where\ncostly n = n+n\nshared :: Num a => a\nshared = costly 5\nignore unused x = x\nobserved x = let go :: forall a. Num a => a -> a; go y = ignore ((shared+shared) :: a) y in go x\n"
        "Source.observed (5::Int)"
        "do { b <- initialize; fromPure1 (observedM b) (5::Int) }" 5 3
    runAction "constrained-sharing-defaulted-use"
        "costly n = n+n\nshared :: Num a => a\nshared = costly 5\nignore unused x = x\nobserved x = ignore shared x\n"
        "Source.observed (5::Int)"
        "do { b <- initialize; fromPure1 (observedM b) (5::Int) }" 5 2
    runAction "constrained-sharing-constructor-dictionaries"
        (unlines ["work n = n+1", "shared :: Foldable t => (Int, t Int)", "shared = (work 5, undefined)",
                  "asList x = case (shared :: (Int,[Int])) of { (n,_) -> n+x }",
                  "asMaybe x = case (shared :: (Int,Maybe Int)) of { (n,_) -> n+x }"])
        "Source.asList 0 + Source.asMaybe 0"
        "do { b <- initialize; i <- fromPure1 (asListM b) (0::Int); j <- fromPure1 (asMaybeM b) (0::Int); pure (i+j) }" 12 4
    runAction "constrained-sharing-all-dictionaries"
        (unlines ["work n = n+n", "shared :: (Num a, Num b) => (a,b)", "shared = (work 5, work 5)",
                  "one x = case (shared :: (Int,Integer)) of { (n,_) -> n+x }",
                  "two x = case (shared :: (Int,Int)) of { (n,_) -> n+x }"])
        "Source.one 0 + Source.two 0"
        "do { b <- initialize; i <- fromPure1 (oneM b) (0::Int); j <- fromPure1 (twoM b) (0::Int); pure (i+j) }" 20 4
    runAction "constrained-sharing-nested-allocations"
        (unlines ["work n = n+n", "expensive n | n == 0 = n+1 | otherwise = expensive (n-n)+1",
                  "outer :: (Eq a, Num a) => a",
                  "outer = let inner :: Num b => b; inner = work 5 in if (inner :: Int) > 0 then expensive 18446744073709551616 else 0",
                  "asInt :: Int -> Int", "asInt x = outer+x", "asInteger :: Integer -> Integer", "asInteger x = outer+x"])
        "if (Source.asInt 0, Source.asInteger 0) == (1,2) then 12 else 0"
        "do { b <- initialize; i <- fromPure1 (asIntM b) (0::Int); j <- fromPure1 (asIntegerM b) (0::Integer); pure (if (i,j) == (1,2) then 12 else 0) }" 12 7
    runExample "injective-value-names"
        "x %% y = x+y\noperator_37_37 x = x+1\nobserved x = (x %% x) + operator_37_37 x\n"
        "observedM" 16 3
    runExample "injective-constructor-names"
        "data R a = a :%: a | Constructor_58_37_58 a\npick (x :%: _) = x\npick (Constructor_58_37_58 x) = x+x\nobserved x = pick (x :%: undefined) + pick (Constructor_58_37_58 x)\n"
        "observedM" 15 3
    runExample "injective-type-and-helper-names"
        "{-# LANGUAGE TypeOperators #-}\nmodule Example where\ndata a :%: b = Pair a b\ndata Constructor_58_37_58 a = Plain a\ndata SharingStep = SourceStep Int\npickPair (Pair x _) = x\npickPlain (Plain x) = x\nobserved x = pickPair (Pair x undefined) + pickPlain (Plain x)\n"
        "observedM" 10 3
    runExample "identifier-escaping-magic-hash"
        "{-# LANGUAGE MagicHash #-}\nmodule Example where\ndata Box# a = Box# a\nreadBox# (Box# x) = x\nobserved x = readBox# (Box# x)\n"
        "observedM" 5 2
    runExample "record-selector-prelude-collision"
        "import Prelude hiding (map)\ndata R = R { map :: Int }\nobserved x = map (R x)\n"
        "observedM" 5 2
    runExample "record-pattern-source-order"
        "data R = R { a :: Int, b :: Int }\nf R { b=0, a=0 } = 1\nf _ = 2\nobserved x = f (R undefined 1)\n"
        "observedM" 2 2
    checkGeneratedWarnings
    checkGeneratedModuleNameDiagnostic
    runExample "composition-partial-map"
        "add a b = a+b\ntwice x = x+x\nobserved x = foldr add 0 (map ((add . twice) x) [x,x])\n"
        "observedM" 30 12
    runExample "composition-prefix"
        "twice x = x+x\nobserved x = head (map ((.) twice twice) [x])\n"
        "observedM" 20 5
    runExample "composition-generalized-partial"
        "add4 a b c d = a+b+c+d\ntwice x = x+x\nobserved x = foldr ((add4 . twice) x x) 0 [x,x]\n"
        "observedM" 40 7
    runExample "composition-nested-partial-sharing"
        "add a b = a+b\ntwice x = x+x\nobserved x = foldr add 0 (map (((add . twice) . twice) x) [x,x])\n"
        "observedM" 50 13
    runExample "composition-captured-operands"
        "add a b = a+b\ntwice x = x+x\nobserved x = foldr add 0 (map (add (twice x) . add (twice (twice x))) [x,x])\n"
        "observedM" 70 16
    runExample "composition-arity-two-right-operand"
        "add a b = a+b\nobserved x = head (head (map ((map . add) x) [[x]]))\n"
        "observedM" 10 6

checkThunkForwarding :: IO ()
checkThunkForwarding = do
    selected <- getArgs
    when (null selected || "thunk-forwarding" `elem` selected) $ do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-thunk-forwarding-"
      putStrLn ("Thunk-forwarding fixtures and logs: " ++ root)
      writeFile (root </> "Example.hs") $ unlines
          [ "{-# LANGUAGE ScopedTypeVariables #-}"
          , "module Example where"
          , "f :: a -> Int"
          , "f x = 42"
          , "g :: a -> Int"
          , "g x = f x"
          , "parenthesized x = f ((x))"
          , "annotated :: forall a. a -> Int"
          , "annotated x = f (x :: a)"
          , "bad :: a -> Int"
          , "bad x = error \"computed argument evaluated\""
          , "computed x = f (bad x)"
          ]
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      writeFile (root </> "Main.hs") $ unlines
          [ "module Main where"
          , "import Generated"
          , "import qualified Example as Source"
          , "import qualified Memorist.Memorist as M"
          , "import qualified Memorist.Base.Thunk as MT"
          , "import Control.Monad (unless)"
          , "check :: String -> M.ThName -> (M.Th Int -> M.MemoristM Int) -> IO ()"
          , "check label expected forward = do"
          , "  let program = do { x <- M.lazy (error \"unused argument evaluated\" :: M.MemoristM Int); forward x }"
          , "      (result, next) = M.runM program M.nzero"
          , "  unless (M.value result == 42) (fail (label ++ \": forwarding changed the result\"))"
          , "  unless (MT.toInteger (M.costAnnot result) == 2) (fail (label ++ \": forwarding changed entry ticks\"))"
          , "  unless (next == expected) (fail (label ++ \": wrong thunk allocation: \" ++ show next))"
          , "main = do"
          , "  unless (Source.g (undefined :: Int) == 42 && Source.computed (undefined :: Int) == 42) (fail \"source oracle failed\")"
          , "  let oneInput = M.nnext M.nzero"
          , "  check \"plain\" oneInput gM"
          , "  check \"parenthesized\" oneInput parenthesizedM"
          , "  check \"annotated\" oneInput annotatedM"
          , "  check \"computed\" (M.nnext oneInput) computedM"
          ]
      _ <- compileAndRun root
      putStrLn "PASS: thunk-forwarding / unused argument / two ticks / no forwarding allocation"

checkThunkMatching :: IO ()
checkThunkMatching = do
    selected <- getArgs
    mapM_ (check selected)
        [ ("thunk-case", ["caseAlias", "caseParenthesized", "caseAnnotated", "caseAs"])
        , ("thunk-guards", ["guardAlias", "guardParenthesized", "guardAnnotated", "guardAs"])
        ]
  where
    check selected (label, functions) = when (null selected || label `elem` selected) $ do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp ("cost-translate-" ++ label ++ "-")
      putStrLn ("Thunk-matching fixtures and logs: " ++ root)
      writeFile (root </> "Example.hs") $ unlines
          [ "{-# LANGUAGE ScopedTypeVariables #-}"
          , "module Example where"
          , "caseAlias x = case x of y -> (x,y)"
          , "caseParenthesized x = case ((x)) of y -> (x,y)"
          , "caseAnnotated :: forall a. a -> (a,a)"
          , "caseAnnotated x = case (x :: a) of y -> (x,y)"
          , "caseAs x = case x of y@_ -> (x,y)"
          , "guardAlias x | y <- x = (x,y)"
          , "guardParenthesized x | y <- ((x)) = (x,y)"
          , "guardAnnotated :: forall a. a -> (a,a)"
          , "guardAnnotated x | y <- (x :: a) = (x,y)"
          , "guardAs x | y@_ <- x = (x,y)"
          ]
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      writeFile (root </> "Main.hs") $ unlines $
          [ "module Main where"
          , "import Generated"
          , "import qualified Example as Source"
          , "import qualified Memorist.Memorist as M"
          , "import qualified Memorist.Base.Thunk as MT"
          , "import Control.Monad (unless)"
          , "check :: String -> (Int -> (Int,Int)) -> (M.Th Int -> M.MemoristM (M.PairT Int Int)) -> IO ()"
          , "check label source make = do"
          , "  unless (source 5 == (5,5) && case source undefined of { (_,_) -> True }) (fail (label ++ \": source oracle failed\"))"
          , "  let program = do { x <- M.lazy (error \"unused match subject evaluated\" :: M.MemoristM Int); result <- make x; pure (M.tname x, result) }"
          , "      (result, next) = M.runM program M.nzero"
          , "      (name, M.PairT a b) = M.value result"
          , "  unless (M.tname a == name && M.tname b == name) (fail (label ++ \": match alias changed thunk identity\"))"
          , "  unless (next == M.nnext M.nzero) (fail (label ++ \": match subject added a forwarding allocation\"))"
          , "  unless (MT.toInteger (M.costAnnot result) == 1) (fail (label ++ \": match preparation changed entry ticks\"))"
          , "  let demanded = do { x <- M.lazy (M.tick >> pure (5::Int)); M.PairT a b <- make x; av <- M.force a; bv <- M.force b; again <- M.force b; pure (av+bv+again) }"
          , "  unless (M.correct demanded (15::Int) && MT.toInteger (M.infcost demanded M.fullDemand) == 2) (fail (label ++ \": repeated demand changed values or sharing ticks\"))"
          , "main = do"
          ] ++ ["  check " ++ show name ++ " Source." ++ name ++ " " ++ name ++ "M" | name <- functions]
      _ <- compileAndRun root
      putStrLn ("PASS: " ++ label ++ " / match alias identities / no forwarding allocation / lazy subjects / entry ticks")

checkAnfDemand :: IO ()
checkAnfDemand = do
    selected <- getArgs
    when (null selected || "anf-demand" `elem` selected) $ do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-anf-demand-"
      putStrLn ("ANF-demand fixtures and logs: " ++ root)
      writeFile (root </> "Example.hs") $ unlines
          [ "module Example where"
          , "data Box = Box Int"
          , "data Record = Record { first :: Int, second :: Int }"
          , "work :: Int -> Int"
          , "work n = n+n"
          , "bad :: a -> a"
          , "bad x = error \"demanded bad computation\""
          , "caseComputed x = case work x of y -> (y,y)"
          , "guardComputed x | y <- work x = (y,y)"
          , "caseConditional, guardConditional, caseBad, guardBad, sourceLet :: Int -> (Int,Int)"
          , "caseConditional x = case (if (undefined :: Bool) then x else x) of y -> (y,y)"
          , "guardConditional x | y <- (if (undefined :: Bool) then x else x) = (y,y)"
          , "caseBad x = case bad x of y -> (y,y)"
          , "guardBad x | y <- bad x = (y,y)"
          , "sourceLet x = let y = x in (y,y)"
          , "caseVariable, caseWildcard, guardVariable, guardWildcard, caseLiteral, guardLiteral :: Int -> Int"
          , "caseVariable x = case x of y -> 7"
          , "caseWildcard x = case x of _ -> 7"
          , "guardVariable x | y <- x = 7"
          , "guardWildcard x | _ <- x = 7"
          , "caseLiteral x = case x of { 0 -> 7; _ -> 8 }"
          , "guardLiteral x | 0 <- x = 7 | otherwise = 8"
          , "caseConstructor, guardConstructor :: Box -> Int"
          , "caseConstructor x = case x of Box _ -> 7"
          , "guardConstructor x | Box _ <- x = 7"
          , "caseAs, guardAs :: Box -> (Box,Box)"
          , "caseAs x = case x of y@(Box _) -> (x,y)"
          , "guardAs x | y@(Box _) <- x = (x,y)"
          , "caseNested, guardNested :: (Box,Box) -> Int"
          , "caseNested x = case x of { (Box 0, Box 0) -> 1; _ -> 2 }"
          , "guardNested x | (Box 0, Box 0) <- x = 1 | otherwise = 2"
          , "caseRecord, guardRecord :: Record -> Int"
          , "caseRecord x = case x of { Record { second = 0, first = 0 } -> 1; _ -> 2 }"
          , "guardRecord x | Record { second = 0, first = 0 } <- x = 1 | otherwise = 2"
          , "caseGuardSequence, guardSequence :: Int -> Int"
          , "caseGuardSequence x = case x of { y | y > 0, 0 <- bad y -> 1; _ -> 2 }"
          , "guardSequence x | x > 0, 0 <- bad x = 1 | otherwise = 2"
          , "ignore :: Int -> Int -> Int"
          , "ignore unused x = x"
          , "conditionalArg, nestedLetArg, scalar, scalarWork, primitiveWork, conditionalWork :: Int -> Int"
          , "conditionalArg x = ignore (if (undefined :: Bool) then x else x) x"
          , "nestedLetArg x = ignore (let y = bad x in y+y) x"
          , "scalar x = x"
          , "scalarWork x = scalar (work x)"
          , "primitiveWork x = work x + work (x+1)"
          , "conditionalWork x = choose (positive x) (work x) (bad x)"
          , "add :: Int -> Int -> Int"
          , "add x y = x+y"
          , "choose :: Bool -> Int -> Int -> Int"
          , "choose c x y = if c then x else y"
          , "positive :: Int -> Bool"
          , "positive x = x > 0"
          , "boolAnd, boolOr :: Bool -> Bool -> Bool"
          , "boolAnd x y = x && y"
          , "boolOr x y = x || y"
          , "shortAnd, shortOr :: Int -> Bool"
          , "shortAnd x = boolAnd (positive x) (positive (work x))"
          , "shortOr x = boolOr (positive x) (positive (work x))"
          ]
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      writeFile (root </> "Main.hs") $ unlines
          [ "module Main where"
          , "import Generated"
          , "import qualified Example as Source"
          , "import qualified Memorist.Memorist as M"
          , "import qualified Memorist.Base.Thunk as MT"
          , "import Control.Exception (SomeException, evaluate, try)"
          , "import Control.Monad (unless)"
          , "assert label ok = unless ok (fail label)"
          , "pass label = putStrLn (\"PASS: \" ++ label)"
          , "checkInt :: String -> Int -> Integer -> Int -> M.MemoristM Int -> IO ()"
          , "checkInt label expected ticks source program = do"
          , "  assert (label ++ \": source value\") (source == expected)"
          , "  assert (label ++ \": generated value\") (M.correct program expected)"
          , "  assert (label ++ \": entry ticks\") (MT.toInteger (M.infcost program M.fullDemand) == ticks)"
          , "  pass label"
          , "checkBool :: String -> Bool -> Integer -> Bool -> M.MemoristM Bool -> IO ()"
          , "checkBool label expected ticks source program = do"
          , "  assert (label ++ \": source value\") (source == expected)"
          , "  assert (label ++ \": generated value\") (M.correct program expected)"
          , "  assert (label ++ \": entry ticks\") (MT.toInteger (M.infcost program M.fullDemand) == ticks)"
          , "  pass label"
          , "fails :: String -> Bool -> Bool -> IO ()"
          , "fails label source generated = do"
          , "  a <- try (evaluate source) :: IO (Either SomeException Bool)"
          , "  b <- try (evaluate generated) :: IO (Either SomeException Bool)"
          , "  case (a,b) of { (Left _,Left _) -> pass label; _ -> fail (label ++ \": source and generated must both fail\") }"
          , "fresh :: String -> (Int -> (Int,Int)) -> (M.Th Int -> M.MemoristM (M.PairT Int Int)) -> IO ()"
          , "fresh label source make = do"
          , "  assert (label ++ \": source spine\") (case source undefined of { (_,_) -> True })"
          , "  let program = do { x <- M.lazy (error \"unused input demanded\" :: M.MemoristM Int); result <- make x; pure (M.tname x,result) }"
          , "      (result,next) = M.runM program M.nzero"
          , "      (name,M.PairT a b) = M.value result"
          , "  assert (label ++ \": fresh shared identity\") (M.tname a /= name && M.tname a == M.tname b)"
          , "  -- nnext counts names only at this naming level, not allocations in suspended bodies."
          , "  assert (label ++ \": one fresh subject or source anchor\") (next == M.nnext (M.nnext M.nzero))"
          , "  assert (label ++ \": preparation ticks\") (MT.toInteger (M.costAnnot result) == 1)"
          , "  assert (label ++ \": skipped fields\") (MT.toInteger (M.infcost' (M.fromPure1 make (5::Int)) (M.PairD M.Skip M.Skip)) == 1)"
          , "  pass label"
          , "repeated :: String -> Int -> Integer -> (Int -> (Int,Int)) -> (M.Th Int -> M.MemoristM (M.PairT Int Int)) -> IO ()"
          , "repeated label expected ticks source make = do"
          , "  assert (label ++ \": source values\") (source 5 == (expected,expected))"
          , "  let program = M.fromPure1 make (5::Int)"
          , "      demanded = do { M.PairT a b <- program; av <- M.force a; bv <- M.force b; again <- M.force a; pure (av+bv+again) }"
          , "  assert (label ++ \": generated values\") (M.correct program (expected,expected) && M.correct demanded (3*expected))"
          , "  assert (label ++ \": one field cost\") (MT.toInteger (M.infcost' program (M.PairD (M.Need expected) M.Skip)) == ticks)"
          , "  assert (label ++ \": both fields cost\") (MT.toInteger (M.infcost' program (M.PairD (M.Need expected) (M.Need expected))) == ticks)"
          , "  assert (label ++ \": repeated force cost\") (MT.toInteger (M.infcost demanded M.fullDemand) == ticks)"
          , "  pass label"
          , "asPattern :: String -> (Source.Box -> (Source.Box,Source.Box)) -> (M.Th BoxT -> M.MemoristM (M.PairT BoxT BoxT)) -> IO ()"
          , "asPattern label source make = do"
          , "  assert (label ++ \": source outer constructors\") (case source (Source.Box undefined) of { (Source.Box _,Source.Box _) -> True })"
          , "  let program = do { field <- M.lazy (error \"as-pattern field demanded\" :: M.MemoristM Int); x <- M.lazy (pure (BoxT field)); result <- make x; pure (M.tname x,result) }"
          , "      (result,next) = M.runM program M.nzero"
          , "      (name,M.PairT a b) = M.value result"
          , "  assert (label ++ \": whole input identity\") (M.tname a == name && M.tname b == name)"
          , "  assert (label ++ \": no forwarding allocation\") (next == M.nnext (M.nnext M.nzero))"
          , "  let demand = M.PairD (M.Need (BoxD M.Skip)) (M.Need (BoxD M.Skip))"
          , "  assert (label ++ \": only outer structure demanded\") (MT.toInteger (M.infcost' (M.fromPure1 make (Source.Box undefined)) demand) == 1)"
          , "  pass label"
          , "chooseProgram :: Bool -> Int -> Int -> M.MemoristM Int"
          , "chooseProgram c x y = do { tc <- M.lazy (pure c); tx <- M.lazy (pure x); ty <- M.lazy (pure y); chooseM tc tx ty }"
          , "main = do"
          , "  mapM_ (\\(label,source,make) -> fresh label source make)"
          , "    [(\"computed case\",Source.caseComputed,caseComputedM), (\"computed guard\",Source.guardComputed,guardComputedM),"
          , "     (\"ignored conditional case\",Source.caseConditional,caseConditionalM), (\"ignored conditional guard\",Source.guardConditional,guardConditionalM),"
          , "     (\"ignored bad case\",Source.caseBad,caseBadM), (\"ignored bad guard\",Source.guardBad,guardBadM),"
          , "     (\"source-written let anchor\",Source.sourceLet,sourceLetM)]"
          , "  -- Matching costs one entry; demanding work adds one, even across repeated forces."
          , "  repeated \"computed case demanded once\" 10 2 Source.caseComputed caseComputedM"
          , "  repeated \"computed guard demanded once\" 10 2 Source.guardComputed guardComputedM"
          , "  repeated \"source-written let reused\" 5 1 Source.sourceLet sourceLetM"
          , "  mapM_ (\\(label,source,make) -> checkInt label 7 1 (source undefined) (M.fromPure1 make (undefined::Int)))"
          , "    [(\"case variable\",Source.caseVariable,caseVariableM), (\"case wildcard\",Source.caseWildcard,caseWildcardM),"
          , "     (\"guard variable\",Source.guardVariable,guardVariableM), (\"guard wildcard\",Source.guardWildcard,guardWildcardM)]"
          , "  mapM_ (\\(label,source,make) -> do"
          , "    checkInt (label ++ \" outer only\") 7 1 (source (Source.Box undefined)) (M.fromPure1 make (Source.Box undefined))"
          , "    fails (label ++ \" demands subject\") (source undefined == 7) (M.correct (M.fromPure1 make (undefined::Source.Box)) (7::Int)))"
          , "    [(\"case constructor\",Source.caseConstructor,caseConstructorM), (\"guard constructor\",Source.guardConstructor,guardConstructorM)]"
          , "  mapM_ (\\(label,source,make) -> do"
          , "    checkInt (label ++ \" match\") 7 1 (source 0) (M.fromPure1 make (0::Int))"
          , "    checkInt (label ++ \" fallback\") 8 1 (source 1) (M.fromPure1 make (1::Int))"
          , "    fails (label ++ \" demands subject\") (source undefined == 7) (M.correct (M.fromPure1 make (undefined::Int)) (7::Int)))"
          , "    [(\"case literal\",Source.caseLiteral,caseLiteralM), (\"guard literal\",Source.guardLiteral,guardLiteralM)]"
          , "  asPattern \"case demanding as-pattern\" Source.caseAs caseAsM"
          , "  asPattern \"guard demanding as-pattern\" Source.guardAs guardAsM"
          , "  mapM_ (\\(label,source,make) -> do"
          , "    checkInt (label ++ \" skips nested bottom\") 2 1 (source (Source.Box 1,Source.Box undefined)) (M.fromPure1 make (Source.Box 1,Source.Box undefined))"
          , "    checkInt (label ++ \" full match\") 1 1 (source (Source.Box 0,Source.Box 0)) (M.fromPure1 make (Source.Box 0,Source.Box 0))"
          , "    fails (label ++ \" reaches nested bottom\") (source (Source.Box 0,Source.Box undefined) == 1) (M.correct (M.fromPure1 make (Source.Box 0,Source.Box undefined)) (1::Int)))"
          , "    [(\"case nested mismatch\",Source.caseNested,caseNestedM), (\"guard nested mismatch\",Source.guardNested,guardNestedM)]"
          , "  mapM_ (\\(label,source,make) -> do"
          , "    checkInt (label ++ \" source order\") 2 1 (source (Source.Record undefined 1)) (M.fromPure1 make (Source.Record undefined 1))"
          , "    checkInt (label ++ \" full match\") 1 1 (source (Source.Record 0 0)) (M.fromPure1 make (Source.Record 0 0))"
          , "    fails (label ++ \" reaches later field\") (source (Source.Record undefined 0) == 1) (M.correct (M.fromPure1 make (Source.Record undefined 0)) (1::Int)))"
          , "    [(\"case reverse record\",Source.caseRecord,caseRecordM), (\"guard reverse record\",Source.guardRecord,guardRecordM)]"
          , "  mapM_ (\\(label,source,make) -> do"
          , "    checkInt (label ++ \" skips later guard\") 2 1 (source (-1)) (M.fromPure1 make (-1::Int))"
          , "    fails (label ++ \" reaches later guard\") (source 1 == 1) (M.correct (M.fromPure1 make (1::Int)) (1::Int)))"
          , "    [(\"case guard sequence\",Source.caseGuardSequence,caseGuardSequenceM), (\"equation guard sequence\",Source.guardSequence,guardSequenceM)]"
          , "  -- Ignoring the argument invokes only the wrapper and ignore, not the suspended body."
          , "  checkInt \"conditional argument suspended\" 5 2 (Source.conditionalArg 5) (M.fromPure1 conditionalArgM (5::Int))"
          , "  checkInt \"nested source-let argument suspended\" 5 2 (Source.nestedLetArg 5) (M.fromPure1 nestedLetArgM (5::Int))"
          , "  -- Scalar return: wrapper + scalar + work. Primitive operands: wrapper + two work calls."
          , "  checkInt \"returned scalar demands work\" 10 3 (Source.scalarWork 5) (M.fromPure1 scalarWorkM (5::Int))"
          , "  checkInt \"primitive operands demand both work calls\" 22 3 (Source.primitiveWork 5) (M.fromPure1 primitiveWorkM (5::Int))"
          , "  fails \"returned scalar demands bottom\" (Source.scalar undefined == 0) (M.correct (M.fromPure1 scalarM (undefined::Int)) (0::Int))"
          , "  fails \"primitive left operand demanded\" (Source.add undefined 5 == 0) (M.correct (M.fromPure2 addM (undefined::Int) (5::Int)) (0::Int))"
          , "  fails \"primitive right operand demanded\" (Source.add 5 undefined == 0) (M.correct (M.fromPure2 addM (5::Int) (undefined::Int)) (0::Int))"
          , "  checkInt \"condition selects only true input\" 5 1 (Source.choose True 5 undefined) (chooseProgram True 5 undefined)"
          , "  checkInt \"condition selects only false input\" 5 1 (Source.choose False undefined 5) (chooseProgram False undefined 5)"
          , "  -- conditionalWork + choose + positive + selected work; the bad branch stays suspended."
          , "  checkInt \"computed condition and selected input\" 10 4 (Source.conditionalWork 5) (M.fromPure1 conditionalWorkM (5::Int))"
          , "  fails \"condition itself demanded\" (Source.choose undefined 5 5 == 5) (M.correct (chooseProgram undefined 5 5) (5::Int))"
          , "  fails \"selected true input demanded\" (Source.choose True undefined 5 == 5) (M.correct (chooseProgram True undefined 5) (5::Int))"
          , "  fails \"selected false input demanded\" (Source.choose False 5 undefined == 5) (M.correct (chooseProgram False 5 undefined) (5::Int))"
          , "  fails \"selected bad computation demanded\" (Source.conditionalWork (-5) == 0) (M.correct (M.fromPure1 conditionalWorkM (-5::Int)) (0::Int))"
          , "  checkBool \"and skips right bottom\" False 1 (Source.boolAnd False undefined) (M.fromPure2 boolAndM False (undefined::Bool))"
          , "  checkBool \"or skips right bottom\" True 1 (Source.boolOr True undefined) (M.fromPure2 boolOrM True (undefined::Bool))"
          , "  -- Short-circuit: wrapper + Boolean function + first positive; reaching the right adds positive + work."
          , "  checkBool \"and demands computed right\" True 5 (Source.shortAnd 5) (M.fromPure1 shortAndM (5::Int))"
          , "  checkBool \"and skips computed right\" False 3 (Source.shortAnd (-5)) (M.fromPure1 shortAndM (-5::Int))"
          , "  checkBool \"or skips computed right\" True 3 (Source.shortOr 5) (M.fromPure1 shortOrM (5::Int))"
          , "  checkBool \"or demands computed right\" False 5 (Source.shortOr (-5)) (M.fromPure1 shortOrM (-5::Int))"
          , "  fails \"and left input demanded\" (Source.boolAnd undefined False == False) (M.correct (M.fromPure2 boolAndM (undefined::Bool) False) False)"
          , "  fails \"and required right input demanded\" (Source.boolAnd True undefined == False) (M.correct (M.fromPure2 boolAndM True (undefined::Bool)) False)"
          , "  fails \"or left input demanded\" (Source.boolOr undefined True == True) (M.correct (M.fromPure2 boolOrM (undefined::Bool) True) True)"
          , "  fails \"or required right input demanded\" (Source.boolOr False undefined == True) (M.correct (M.fromPure2 boolOrM False (undefined::Bool)) True)"
          ]
      _ <- compileAndRun root
      putStrLn "PASS: anf-demand / fresh suspension, explicit anchors, ordered matching, scalar and Boolean demand"

checkThunkFields :: IO ()
checkThunkFields = do
    selected <- getArgs
    when (null selected || "thunk-fields" `elem` selected) $ do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-thunk-fields-"
      putStrLn ("Thunk-field fixtures and logs: " ++ root)
      writeFile (root </> "Example.hs") $ unlines
          [ "module Example where"
          , "data Box a = Box { first :: a, second :: a }"
          , "constructor x = Box x x"
          , "record x = Box { second = (x), first = x }"
          , "tuple x = (x,x)"
          , "list x = [x,x]"
          , "cons x xs = x : xs"
          , "bad :: a -> a"
          , "bad x = error \"computed field evaluated\""
          , "mixed x = Box x (bad x)"
          ]
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      writeFile (root </> "Main.hs") $ unlines
          [ "module Main where"
          , "import Generated"
          , "import qualified Memorist.Memorist as M"
          , "import qualified Memorist.Data.List as L"
          , "import qualified Memorist.Base.Thunk as MT"
          , "import Control.Monad (unless)"
          , "observe :: (M.Th Int -> M.MemoristM a) -> (M.ThName, a)"
          , "observe make = M.value (M.eval1 (do { x <- M.lazy (error \"unused field evaluated\" :: M.MemoristM Int); result <- make x; pure (M.tname x, result) }))"
          , "same label expected fields = unless (all ((== expected) . M.tname) fields) (fail (label ++ \": field thunk identity changed\"))"
          , "checkBox label make = let (name, BoxT a b) = observe make in same label name [a,b]"
          , "main = do"
          , "  checkBox \"constructor\" constructorM"
          , "  checkBox \"record\" recordM"
          , "  let (pairName, M.PairT a b) = observe tupleM"
          , "  same \"tuple\" pairName [a,b]"
          , "  let (listName, L.ConsT h t) = observe listM"
          , "      L.ConsT h2 _ = M.tval t"
          , "  same \"list\" listName [h,h2]"
          , "  let consProgram = do { x <- M.lazy (error \"unused head evaluated\" :: M.MemoristM Int); xs <- M.lazy (error \"unused tail evaluated\" :: M.MemoristM (L.ListT Int)); result <- consM x xs; pure (M.tname x, M.tname xs, result) }"
          , "      (headName, tailName, L.ConsT ch ct) = M.value (M.eval1 consProgram)"
          , "  same \"cons head\" headName [ch]"
          , "  same \"cons tail\" tailName [ct]"
          , "  let (mixedName, BoxT reused computed) = observe mixedM"
          , "  same \"mixed existing field\" mixedName [reused]"
          , "  unless (M.tname computed /= mixedName) (fail \"computed field did not receive a fresh thunk\")"
          , "  unless (MT.toInteger (M.infcost' (M.fromPure1 mixedM (5::Int)) (BoxD M.Skip M.Skip)) == 1) (fail \"computed field was evaluated before demand\")"
          ]
      _ <- compileAndRun root
      putStrLn "PASS: thunk-fields / constructor, record, tuple, list identities / suspended computed fields"

checkThunkCaptures :: IO ()
checkThunkCaptures = do
    selected <- getArgs
    when (null selected || "thunk-captures" `elem` selected) $ do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-thunk-captures-"
      putStrLn ("Thunk-capture fixtures and logs: " ++ root)
      writeFile (root </> "Example.hs") $ unlines
          [ "module Example where"
          , "pair a b = (a,b)"
          , "keep x = x"
          , "partial x xs = map (pair x) xs"
          , "composed x xs = map (pair x . keep) xs"
          ]
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      writeFile (root </> "Main.hs") $ unlines
          [ "module Main where"
          , "import Generated"
          , "import qualified Memorist.Memorist as M"
          , "import qualified Memorist.Data.List as L"
          , "import Control.Monad (unless)"
          , "observe :: (M.Th Int -> M.Th (L.ListT Int) -> M.MemoristM (L.ListT (M.PairT Int Int))) -> (M.ThName, L.ListT (M.PairT Int Int))"
          , "observe make = M.value (M.eval1 (do { x <- M.lazy (error \"unused captured argument evaluated\" :: M.MemoristM Int); nil <- M.lazy (pure L.NilT); xs <- M.lazy (pure (L.ConsT x nil)); result <- make x xs; pure (M.tname x, result) }))"
          , "check label make intermediate = do"
          , "  let (name, L.ConsT first _) = observe make"
          , "      M.PairT captured supplied = M.tval first"
          , "  unless (M.tname captured == name) (fail (label ++ \": captured argument was re-thunked\"))"
          , "  unless ((M.tname supplied /= name) == intermediate) (fail (label ++ \": wrong supplied/intermediate thunk identity\"))"
          , "main = do"
          , "  check \"partial\" partialM False"
          , "  check \"composition\" composedM True"
          ]
      _ <- compileAndRun root
      putStrLn "PASS: thunk-captures / partial and composition captures / retained composition intermediate"

checkThunkSharing :: IO ()
checkThunkSharing = do
    selected <- getArgs
    when (null selected || "thunk-sharing" `elem` selected) $ do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-thunk-sharing-"
      putStrLn ("Thunk-sharing fixtures and logs: " ++ root)
      writeFile (root </> "Example.hs") $ unlines
          [ "module Example where"
          , "work n = n+n"
          , "shared :: Num a => a"
          , "shared = work 5"
          , "same u = (shared :: Int, shared :: Int)"
          , "different u = (shared :: Int, shared :: Integer)"
          , "local x = let y = work x in (y,y)"
          ]
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      writeFile (root </> "Main.hs") $ unlines
          [ "module Main where"
          , "import Generated"
          , "import qualified Example as Source"
          , "import qualified Memorist.Memorist as M"
          , "import qualified Memorist.Base.Thunk as MT"
          , "import Control.Monad (unless)"
          , "observeShared = do { bindings <- initialize; u <- M.lazy (pure ()); a <- sameM bindings u; b <- differentM bindings u; pure (a,b) }"
          , "observeLocal = do { bindings <- initialize; M.fromPure1 (localM bindings) (5::Int) }"
          , "demanded = do { (M.PairT a b, M.PairT c d) <- observeShared; a' <- M.force a; b' <- M.force b; c' <- M.force c; d' <- M.force d; pure (a'+b'+c'+fromInteger d') }"
          , "demandedLocal = do { M.PairT a b <- observeLocal; a' <- M.force a; b' <- M.force b; pure (a'+b') }"
          , "main = do"
          , "  unless (Source.same () == (10,10) && Source.different () == (10,10)) (fail \"source oracle failed\")"
          , "  let (M.PairT a b, M.PairT c d) = M.value (M.eval1 observeShared)"
          , "  unless (M.tname a == M.tname b && M.tname a == M.tname c) (fail \"same dictionary specialization was re-thunked\")"
          , "  unless (M.tname c /= M.tname d) (fail \"different dictionary specializations share a name\")"
          , "  let M.PairT l r = M.value (M.eval1 observeLocal)"
          , "  unless (M.tname l == M.tname r) (fail \"local shared binding was re-thunked\")"
          , "  unless (M.correct demanded (40::Int) && MT.toInteger (M.infcost demanded M.fullDemand) == 4) (fail \"dictionary-sensitive sharing changed values or ticks\")"
          , "  unless (M.correct demandedLocal (20::Int) && MT.toInteger (M.infcost demandedLocal M.fullDemand) == 2) (fail \"local sharing changed values or ticks\")"
          ]
      _ <- compileAndRun root
      putStrLn "PASS: thunk-sharing / local and dictionary-sensitive thunk identities / at-most-once ticks"

checkGeneratedModuleNameDiagnostic :: IO ()
checkGeneratedModuleNameDiagnostic = do
    selected <- getArgs
    when (null selected || "generated-module-name-diagnostic" `elem` selected) $ do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-generated-parse-"
      let source = root </> "Example.hs"; output = root </> "bad-name.hs"
      writeFile source "module Example where\nf x = x\n"
      (status,out,err) <- readProcessWithExitCode "cost-translate" [source,output] ""
      logText <- readFile (output ++ ".log")
      published <- doesFileExist output
      unless (status /= ExitSuccess && not published &&
              "generated module name is invalid" `isInfixOf` (out ++ err) &&
              not ("unsupported source" `isInfixOf` logText) && "Failed candidate:" `isInfixOf` logText) $
          fail ("wrong generated module-name diagnostic: " ++ logText)
      putStrLn "PASS: invalid generated module names retain their category and candidate"

checkGeneratedWarnings :: IO ()
checkGeneratedWarnings = do
    selected <- getArgs
    when (null selected || "generated-warnings" `elem` selected) $ do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-warnings-"
      writeFile (root </> "Example.hs") $ unlines
          [ "module Example where"
          , "data Pair = Pair { first :: Int, second :: Int }"
          , "unbox :: Pair -> Int"
          , "unbox Pair { first = x, second = _ } = x"
          , "captured :: Int -> Int"
          , "captured x = let y = x+x; add z = y+z in add x+add x"
          , "choose :: Int -> Int"
          , "choose 0 = 0"
          , "choose x = x"
          , "composed :: Int -> Int"
          , "composed partial = (captured . captured) partial"
          , "singleton :: Int -> [Int]"
          , "singleton x = [x]"
          , "stringPattern :: Int -> String -> Int"
          , "stringPattern field0 \"a\" = field0"
          , "stringPattern field0 _ = field0"
          ]
      result <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure result
      writeFile (root </> "Minimal.hs") "module Minimal where\nidentity :: Int -> Int\nidentity x = x\n"
      minimalResult <- translateFile (root </> "Minimal.hs") (root </> "MinimalGenerated.hs")
      either fail pure minimalResult
      generated <- readFile (root </> "Generated.hs")
      minimal <- readFile (root </> "MinimalGenerated.hs")
      checkGeneratedLayout "Generated.hs" generated
      checkGeneratedLayout "MinimalGenerated.hs" minimal
      unless (all (`isInfixOf` generated) ["capturedM x", "add z", "chooseM arg0", "nextEquation0", "partial'' <-"]) $
          fail "generated local names do not preserve readable source names"
      when (any (`isInfixOf` generated) ["_arg", "argument_", "partial0", "arg0'"]) $
          fail "generated output contains opaque local names"
      let banners = filter ("-- ==== " `isPrefixOf`) (lines generated)
          expectedBanners =
              [ "-- ==== Common: sharing and scalar support ===="
              , "-- ==== Common: foldable support ===="
              , "-- ==== Common: datatype conversion and demand support ===="
              , "-- ==== Module-specific: source datatypes and conversions from Example ===="
              , "-- ==== Module-specific: translated bindings from Example ===="
              ]
      unless (banners == expectedBanners) $
          fail ("wrong generated section banners: " ++ show banners)
      unless ("{-# OPTIONS_GHC -Wno-orphans -Wno-simplifiable-class-constraints #-}" `isInfixOf` generated) $
          fail "generated warning suppressions are missing"
      when (any (`isInfixOf` minimal) ["Data.Kind", "Data.Proxy", "GHC.TypeLits", "qualified Minimal as Source"]) $
          fail "minimal generated module has unnecessary imports"
      (compiled,out,err) <- readProcessWithExitCode "ghc"
          ["-v0", "-fno-code", "-fforce-recomp", "-Wall", "-Werror",
            "-package", "MemoristANT", "-package", "QuickCheck", "-i" ++ root, root </> "Generated.hs"] ""
      writeFile (root </> "warnings.log") (out ++ err)
      unless (compiled == ExitSuccess) (fail (out ++ err))
      (minimalCompiled,minimalOut,minimalErr) <- readProcessWithExitCode "ghc"
          ["-v0", "-fno-code", "-fforce-recomp", "-Wall", "-Werror",
            "-package", "MemoristANT", "-package", "QuickCheck", "-i" ++ root, root </> "MinimalGenerated.hs"] ""
      writeFile (root </> "minimal-warnings.log") (minimalOut ++ minimalErr)
      unless (minimalCompiled == ExitSuccess) (fail (minimalOut ++ minimalErr))
      hlint <- findExecutable "hlint"
      case hlint of
          Nothing -> putStrLn "SKIP: generated code HLint check (hlint unavailable)"
          Just command -> do
              (linted,lintOut,lintErr) <- readProcessWithExitCode command [root </> "Generated.hs", root </> "MinimalGenerated.hs"] ""
              writeFile (root </> "hlint.log") (lintOut ++ lintErr)
              unless (linted == ExitSuccess) (fail (lintOut ++ lintErr))
      putStrLn "PASS: generated layout, banners, minimal/datatype imports, -Wall -Werror, and optional HLint"

checkGeneratedLayout :: FilePath -> String -> IO ()
checkGeneratedLayout name source = do
    unless (hasOneFinalNewline source) $
        fail (name ++ ": expected exactly one final newline")
    when ("\n\n\n" `isInfixOf` source) $
        fail (name ++ ": found a run of three newlines")
    unless (all noTrailingWhitespace sourceLines) $
        fail (name ++ ": found trailing whitespace")
    moduleLine <- exactlyOne "module header" (positions ("module " `isPrefixOf`) sourceLines)
    let plainImports = positions isPlainImport sourceLines
        qualifiedImports = positions ("import qualified " `isPrefixOf`) sourceLines
        declarations = positions isTopLevelDeclaration sourceLines
    case (plainImports, qualifiedImports, reverse plainImports, reverse qualifiedImports) of
        (firstPlain:_, firstQualified:_, lastPlain:_, lastQualified:_) -> do
            unless (firstPlain == moduleLine + 2 && sourceLines !! (moduleLine + 1) == "") $
                fail (name ++ ": expected one blank line after the module header")
            unless (consecutive plainImports && consecutive qualifiedImports &&
                    firstQualified == lastPlain + 2 && sourceLines !! (lastPlain + 1) == "") $
                fail (name ++ ": expected plain and qualified import groups separated by one blank line")
            unless (importLines plainImports == sort (importLines plainImports) &&
                    importLines qualifiedImports == sort (importLines qualifiedImports)) $
                fail (name ++ ": expected alphabetically sorted import groups")
            unless (lastQualified + 1 < length sourceLines && sourceLines !! (lastQualified + 1) == "") $
                fail (name ++ ": expected a blank line after imports")
        _ -> fail (name ++ ": expected plain and qualified import groups")
    when (null declarations) $
        fail (name ++ ": expected at least one top-level declaration")
    case [lineNumber | lineNumber <- declarations,
                       lineNumber == 0 || sourceLines !! (lineNumber - 1) /= "",
                       not (precededBySignature lineNumber)] of
        [] -> pure ()
        lineNumber:_ -> fail (name ++ ": expected one blank line before top-level declaration at line " ++ show (lineNumber + 1))
  where
    sourceLines = lines source
    noTrailingWhitespace line = null line || not (isSpace (last line))
    isPlainImport line = "import " `isPrefixOf` line && not ("import qualified " `isPrefixOf` line)
    isTopLevelDeclaration line@(first:_) = not (isSpace first) &&
        not (any (`isPrefixOf` line) ["{-#", "--", "module ", "import "])
    isTopLevelDeclaration [] = False
    positions predicate = map fst . filter (predicate . snd) . zip [0..]
    importLines = map (sourceLines !!)
    precededBySignature lineNumber = case
        reverse (filter isTopLevelDeclaration (takeWhile (not . null) (reverse (take lineNumber sourceLines)))) of
        [signature] -> "::" `isInfixOf` signature
        _ -> False
    consecutive indexes = and (zipWith (\a b -> b == a + 1) indexes (drop 1 indexes))
    exactlyOne _ [lineNumber] = pure lineNumber
    exactlyOne label found = fail (name ++ ": expected one " ++ label ++ ", found " ++ show (length found))
    hasOneFinalNewline text = case reverse text of
        '\n':previous:_ -> previous /= '\n'
        ['\n'] -> True
        _ -> False

checkHigherKindedDemands :: IO ()
checkHigherKindedDemands = do
    selected <- getArgs
    when (null selected || "higher-kinded-demands" `elem` selected) $ do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-higher-demands-"
      putStrLn ("Higher-kinded fixtures and logs: " ++ root)
      writeFile (root </> "Example.hs") $ unlines
        ["{-# LANGUAGE DataKinds, KindSignatures, PolyKinds, TypeOperators #-}", "module Example where",
         "data Higher f a = Higher (f a)", "data Fix f = In (f (Fix f))",
         "data Nest f = Nest (Maybe (f (Nest f)))", "data Layer a = Layer a",
         "data Mixed f a = Leaf a | Branch (f (Mixed f [a]))",
         "data Index = Key | Index :#: Index", "data Tagged (i :: Index) = Tagged Int",
         "data Tag (i :: k) = Tag Int",
         "data Numeric f = Numeric (f 0)", "data Named f = Named (f \"key\")",
         "data Character f = Character (f 'x')", "data Selected f = Selected (f 'Key)",
         "data Operated f = Operated (f ('Key ':#: 'Key))",
         "readNumeric (Numeric (Tag x)) = x", "readNamed (Named (Tag x)) = x",
         "readCharacter (Character (Tag x)) = x", "readSelected (Selected (Tag x)) = x",
         "readOperated (Operated (Tag x)) = x",
         "indexKinds x = readNumeric (Numeric (Tag x)) + readNamed (Named (Tag x)) + readCharacter (Character (Tag x)) + readSelected (Selected (Tag x)) + readOperated (Operated (Tag x))",
         "forest n = In [forest (n+1), forest (n+2)]",
         "nested n = Nest (Just [nested (n+1)])", "tagged x = Higher (Tagged x)"]
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      writeFile (root </> "Main.hs") $ unlines
        ["{-# LANGUAGE DataKinds #-}", "module Main where", "import Generated",
         "import qualified Example as S", "import Memorist.Memorist", "import Memorist.Data.List",
         "import qualified Memorist.Base.Thunk as MT", "import Test.QuickCheck", "import Control.Monad (unless, replicateM_)",
         "import System.Timeout (timeout)",
         "program = fromPure1 forestM (5::Int)", "tree = value (eval1 program)",
         "nodes :: FixD ListD -> Integer", "nodes (InD ds) = 1 + listNodes ds",
         "listNodes Skip = 0", "listNodes (Need NilD) = 0",
         "listNodes (Need (ConsD h t)) = childNodes h + listNodes t",
         "childNodes Skip = 0", "childNodes (Need d) = nodes d",
         "compatible (InT xs) (InD ds) = listCompatible (tval xs) ds",
         "listCompatible _ Skip = True", "listCompatible NilT (Need NilD) = True",
         "listCompatible (ConsT h t) (Need (ConsD dh dt)) = childCompatible h dh && listCompatible (tval t) dt",
         "listCompatible _ _ = False", "childCompatible _ Skip = True",
         "childCompatible t (Need d) = compatible (tval t) d",
         "nestedNodes (NestD ds) = 1 + maybeNodes ds", "maybeNodes Skip = 0",
         "maybeNodes (Need NothingD) = 0", "maybeNodes (Need (JustD ds)) = nestedList ds",
         "nestedList Skip = 0", "nestedList (Need NilD) = 0",
         "nestedList (Need (ConsD h t)) = nestedChild h + nestedList t",
         "nestedChild Skip = 0", "nestedChild (Need d) = nestedNodes d",
         "assert label ok = unless ok (fail label)",
         "indexed = fromPure1 taggedM (5::Int) :: MemoristM (HigherT TaggedT 'S.Key)",
         "main = do",
         "  assert \"literal index kinds value\" (S.indexKinds 5 == 25 && correct (fromPure1 indexKindsM (5::Int)) (25::Int))",
         "  assert \"literal index kinds cost\" (MT.toInteger (infcost (fromPure1 indexKindsM (5::Int)) fullDemand) == 6)",
         "  let converted = value (eval1 (reprM (S.Higher (S.Layer (5::Int))) :: MemoristM (HigherT LayerT Int)))",
         "  assert \"custom constructor conversion\" (case (abstract converted :: S.Higher S.Layer Int) of { S.Higher (S.Layer n) -> n == 5 })",
         "  assert \"index value\" (case (abstract (value (eval1 indexed)) :: S.Higher S.Tagged 'S.Key) of { S.Higher (S.Tagged n) -> n == 5 })",
         "  custom <- generate (genHigherDemand 12 converted)", "  print custom",
         "  let deepProgram = reprM (S.Higher (S.In [] :: S.Fix [])) :: MemoristM (HigherT FixT ListT)",
         "  let deep = value (eval1 deepProgram)",
         "  assert \"constructor taking a constructor argument\" (case (abstract deep :: S.Higher S.Fix []) of { S.Higher (S.In []) -> True; _ -> False })",
         "  deepDemand <- generate (genHigherDemand 12 deep)",
         "  print (shrinkHigherDemand deep deepDemand)",
         "  assert \"conversion dictionaries add no ticks\" (MT.toInteger (infcost' deepProgram deepDemand) == 0)",
         "  let finite = value (eval1 (reprM (S.Nest (Just []) :: S.Nest []) :: MemoristM (NestT ListT)))",
         "  assert \"nested recursive conversion\" (case (abstract finite :: S.Nest []) of { S.Nest (Just []) -> True; _ -> False })",
         "  let mixedProgram = reprM (S.Branch (Just (S.Branch (Just (S.Leaf [[5::Int]]))))) :: MemoristM (MixedT MaybeT Int)",
         "  let mixed = value (eval1 mixedProgram)",
         "  assert \"non-regular higher-kinded conversion\" (case (abstract mixed :: S.Mixed Maybe Int) of { S.Branch (Just (S.Branch (Just (S.Leaf n)))) -> n == [[5]]; _ -> False })",
         "  mixedDemand <- generate (genMixedDemand 16 mixed)",
         "  assert \"non-regular higher-kinded demands and shrinks\" (all (\\d -> MT.toInteger (infcost' mixedProgram d) == 0) (mixedDemand : shrinkMixedDemand mixed mixedDemand))",
         "  zero <- generate (genFixDemand 0 (InT (error \"skipped field evaluated\") :: FixT ListT))",
         "  assert \"zero budget\" (zero == InD Skip)",
         "  outcome <- timeout 10000000 $ replicateM_ 100 $ do",
         "    budget <- generate (chooseInt (0,25))",
         "    d <- generate (resize budget (arbDemandFrom tree))",
         "    let shrunk = shrinkFixDemand tree d",
         "    assert \"compatible demand and shrinks\" (compatible tree d && all (compatible tree) shrunk)",
         "    assert \"finite budget\" (nodes d <= fromIntegral budget + 1)",
         "    assert \"one tick per observed producer node\" (MT.toInteger (infcost' program d) == nodes d)",
         "    assert \"shrunk costs\" (all (\\s -> MT.toInteger (infcost' program s) == nodes s && nodes s <= nodes d) shrunk)",
         "    indexDemand <- generate (genHigherDemand budget (value (eval1 indexed)))",
         "    assert \"non-Type index cost\" (MT.toInteger (infcost' indexed indexDemand) == 1)",
         "    let nestedProgram = fromPure1 nestedM (5::Int)",
         "    nestedDemand <- generate (genNestDemand budget (value (eval1 nestedProgram)))",
         "    assert \"nested constructor application cost\" (MT.toInteger (infcost' nestedProgram nestedDemand) == nestedNodes nestedDemand)",
         "  case outcome of { Nothing -> fail \"higher-kinded demand generation timed out\"; Just () -> pure () }"]
      _ <- compileAndRun root
      putStrLn "PASS: higher-kinded conversion, custom constructors, index kinds, infinite bounded demands and shrinking"

-- Keep all three original regressions live; report every failure if they recur.
checkHigherKindedAudit :: IO ()
checkHigherKindedAudit = do
    failures <- mapM check
        [("higher-kinded-datatype-field",
          "data Higher f a = Higher a (f a)\nwrap x ys = Higher x ys\nfirst (Higher _ (x:_)) = x\nobserved x = first (wrap undefined [x])\n"),
         ("higher-kinded-recursive-field",
          "data Fix f = In (f (Fix f))\nroll x = In x\nisEmpty (In []) = True\nisEmpty (In (_:_)) = False\nobserved x = if isEmpty (roll []) then x else 0\n"),
         ("higher-kinded-polykinded-field",
          "data Higher f a = Higher (f a)\nwrap x = Higher x\nfirst (Higher (x:_)) = x\nobserved x = first (wrap [x])\n")]
    unless (all null failures) (fail (unlines (filter (not . null) failures)))
  where
    check (label, program) = do
        result <- try (runExample label program "observedM" 5 3) :: IO (Either SomeException ())
        pure (either (\e -> label ++ ": " ++ displayException e) (const "") result)

checkPatternScaling :: IO ()
checkPatternScaling = do
    selected <- getArgs
    when (null selected || "pattern-scaling" `elem` selected) $ do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-patterns-"
      writeFile (root </> "Example.hs") $ unlines $
          "module Example where" : ["choose " ++ show n ++ " " ++ show n ++ " = " ++ show (100+n) | n <- [0::Int ..9]] ++ ["choose _ _ = 0"]
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      generated <- readFile (root </> "Generated.hs")
      unless (length generated < 40000) (fail ("fallback duplication: " ++ show (length generated) ++ " bytes"))
      writeFile (root </> "Main.hs") $ unlines
        ["module Main where", "import Generated", "import qualified Example as S", "import Memorist.Memorist", "import qualified Memorist.Base.Thunk as MT", "import Control.Monad (unless)",
         "main = mapM_ check [(a,b) | a <- [-1..11::Int], b <- [-1..11::Int]]",
         "check (a,b) = unless (correct (fromPure2 chooseM a b) (S.choose a b :: Int) && MT.toInteger (infcost (fromPure2 chooseM a b :: MemoristM Int) fullDemand) == 1) (fail (show (a,b)))"]
      _ <- compileAndRun root
      putStrLn ("PASS: pattern-scaling / 169 values and costs / " ++ show (length generated) ++ " bytes")

compileAndRun :: FilePath -> IO String
compileAndRun root = do
    (compiled,out,err) <- readProcessWithExitCode "ghc"
        ["-v0", "-package", "MemoristANT", "-package", "QuickCheck", "-i" ++ root, root </> "Main.hs", "-o", root </> "driver"] ""
    writeFile (root </> "compile.log") (out ++ err)
    unless (compiled == ExitSuccess) (fail err)
    (ran,result,errors) <- readProcessWithExitCode (root </> "driver") [] ""
    writeFile (root </> "run.log") (result ++ errors)
    unless (ran == ExitSuccess) (fail (result ++ errors))
    pure result

-- Compile one supported source module, then vary values and demands in its driver.
-- Every sample visits all four masks; shrinking preserves the finite input bounds.
checkAnfProperties :: IO ()
checkAnfProperties = do
    selected <- getArgs
    when (null selected || "anf-properties" `elem` selected) $ do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-anf-properties-"
      putStrLn ("ANF property fixtures and logs: " ++ root)
      writeFile (root </> "Example.hs") $ unlines
          [ "module Example where"
          , "work :: Int -> Int"
          , "work x = x+x"
          , "keep x = x"
          , "returnedShared x = let y = work x in (keep y,keep y)"
          , "alias x = (x,x)"
          , "separate x = (work x,work x)"
          , "localPair x = let y = work x in (y,y)"
          , "caseLocal x = let y = work x in case y of z -> (y,z)"
          , "caseAnnotated x = let y = work x in case (y :: Int) of z -> (y,z)"
          , "guardLocal x | let y = work x, z <- y = (y,z)"
          , "guardAnnotated x | let y = work x, z <- (y :: Int) = (y,z)"
          , "computedCase x = case work x of y -> (y,y)"
          , "computedGuard x | y <- work x = (y,y)"
          , "pair a b = (a,b)"
          , "captured x xs = map (pair x) xs"
          , "computedCapture x xs = map (pair (work x)) xs"
          , "specialized x xs = map ((pair . work) x) xs"
          , "nestedSpecialized x xs = map (((pair . work) . work) x) xs"
          ]
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      writeFile (root </> "Main.hs") $ unlines
          [ "module Main where"
          , "import Generated"
          , "import qualified Example as Source"
          , "import qualified Memorist.Memorist as M"
          , "import qualified Memorist.Data.List as L"
          , "import qualified Memorist.Base.Thunk as MT"
          , "import Test.QuickCheck"
          , "import Test.QuickCheck.Random (mkQCGen)"
          , "import Control.Monad (unless, replicateM)"
          , "type PairFn = M.Th Int -> M.MemoristM (M.PairT Int Int)"
          , "type ListFn = M.Th Int -> M.Th (L.ListT Int) -> M.MemoristM (L.ListT (M.PairT Int Int))"
          , "pairFixtures :: [(String, Int -> (Int,Int), PairFn, Int)]"
          , "pairFixtures = [(\"alias\", Source.alias, aliasM, 0), (\"separate\", Source.separate, separateM, 2), (\"local\", Source.localPair, localPairM, 1), (\"case-local\", Source.caseLocal, caseLocalM, 1), (\"case-annotated\", Source.caseAnnotated, caseAnnotatedM, 1), (\"guard-local\", Source.guardLocal, guardLocalM, 1), (\"guard-annotated\", Source.guardAnnotated, guardAnnotatedM, 1), (\"computed-case\", Source.computedCase, computedCaseM, 1), (\"computed-guard\", Source.computedGuard, computedGuardM, 1)]"
          , "listFixtures :: [(String, Int -> [Int] -> [(Int,Int)], ListFn, Int)]"
          , "listFixtures = [(\"captured\", Source.captured, capturedM, 0), (\"computed-capture\", Source.computedCapture, computedCaptureM, 1), (\"specialized-composition\", Source.specialized, specializedM, 1), (\"nested-specialized-composition\", Source.nestedSpecialized, nestedSpecializedM, 2)]"
          , "masks :: [(Bool,Bool)]"
          , "masks = [(False,False), (True,False), (False,True), (True,True)]"
          , "orderedMasks start = take 4 (drop start (cycle masks))"
          , "inputGen :: Gen (Int,Int,Int,Int)"
          , "inputGen = (,,,) <$> chooseInt (-40,40) <*> chooseInt (0,3) <*> chooseInt (1,8) <*> chooseInt (0,5)"
          , "shrinkInputs :: (Int,Int,Int,Int) -> [(Int,Int,Int,Int)]"
          , "shrinkInputs sample = filter bounded (shrink sample) where bounded (n,start,repeats,width) = abs n <= 40 && start >= 0 && start <= 3 && repeats >= 1 && repeats <= 8 && width >= 0 && width <= 5"
          , "cost :: M.MemoristM Int -> Integer"
          , "cost action = MT.toInteger (M.infcost action M.fullDemand)"
          , "observePure left right (a,b) = (if left then a else 0) + (if right then b else 0)"
          , "observePair :: Bool -> Bool -> M.PairT Int Int -> M.MemoristM Int"
          , "observePair left right (M.PairT a b) = do"
          , "  av <- if left then M.force a else pure 0"
          , "  bv <- if right then M.force b else pure 0"
          , "  pure (av+bv)"
          , "pairDemand :: Bool -> Bool -> (Int,Int) -> M.PairD Int Int"
          , "pairDemand left right (a,b) = M.PairD (if left then M.Need a else M.Skip) (if right then M.Need b else M.Skip)"
          , "checkPair :: Int -> Int -> (Bool,Bool) -> (String, Int -> (Int,Int), PairFn, Int) -> Property"
          , "checkPair n repeats (left,right) (label,source,make,workCopies) = counterexample (label ++ \" / mask \" ++ show (left,right)) $"
          , "  let (inputName,aName,bName) = M.value (M.eval1 (do { x <- M.lazy (pure n); M.PairT a b <- make x; pure (M.tname x,M.tname a,M.tname b) }))"
          , "      namesOK = if workCopies == 0 then aName == inputName && bName == inputName else aName /= inputName && bName /= inputName && ((aName == bName) == (workCopies == 1))"
          , "      observed = do { result <- M.fromPure1 make n; values <- replicateM repeats (observePair left right result); pure (sum values) }"
          , "      expected = repeats * observePure left right (source n)"
          , "      workEntries = if workCopies == 0 then 0 else if workCopies == 1 then fromEnum (left || right) else fromEnum left + fromEnum right"
          , "      expectedTicks = 1 + toInteger workEntries"
          , "  in conjoin [counterexample (show (inputName,aName,bName)) (namesOK === True), M.correct (M.fromPure1 make n) (source n) === True, M.correct observed expected === True, cost observed === expectedTicks, MT.toInteger (M.infcost' (M.fromPure1 make n) (pairDemand left right (source n))) === expectedTicks]"
          , "checkReturnedSharing :: Int -> Int -> (Bool,Bool) -> Property"
          , "checkReturnedSharing n repeats (left,right) = counterexample (\"returned shared variable / mask \" ++ show (left,right)) $"
          , "  let program = M.fromPure1 returnedSharedM n"
          , "      observed = do { result <- program; values <- replicateM repeats (observePair left right result); pure (sum values) }"
          , "      source = Source.returnedShared n"
          , "      expected = repeats * observePure left right source"
          , "      expectedTicks = 1 + toInteger (fromEnum left + fromEnum right + fromEnum (left || right))"
          , "  -- Each demanded keep call ticks, but both return the same shared work result."
          , "  in conjoin [M.correct program source === True, M.correct observed expected === True, cost observed === expectedTicks, MT.toInteger (M.infcost' program (pairDemand left right source)) === expectedTicks]"
          , "inputNames L.NilT = []"
          , "inputNames (L.ConsT h t) = M.tname h : inputNames (M.tval t)"
          , "outputNames L.NilT = []"
          , "outputNames (L.ConsT h t) = let M.PairT a b = M.tval h in (M.tname a,M.tname b) : outputNames (M.tval t)"
          , "observeList :: Bool -> Bool -> L.ListT (M.PairT Int Int) -> M.MemoristM Int"
          , "observeList _ _ L.NilT = pure 0"
          , "observeList left right (L.ConsT h t) = do"
          , "  pair <- M.force h"
          , "  here <- observePair left right pair"
          , "  rest <- M.force t >>= observeList left right"
          , "  pure (here+rest)"
          , "listDemand :: Bool -> Bool -> [(Int,Int)] -> L.ListD (M.PairD Int Int)"
          , "listDemand left right = foldr (\\pair rest -> L.ConsD (M.Need (pairDemand left right pair)) (M.Need rest)) L.NilD"
          , "checkList :: Int -> Int -> Int -> (Bool,Bool) -> (String, Int -> [Int] -> [(Int,Int)], ListFn, Int) -> Property"
          , "checkList n repeats width (left,right) (label,source,make,workEntries) = counterexample (label ++ \" / mask \" ++ show (left,right)) $"
          , "  let inputs = [n+1 .. n+width]"
          , "      (inputName,supplied,names) = M.value (M.eval1 (do { x <- M.lazy (pure n); xs <- M.reprM inputs; txs <- M.lazy (pure xs); output <- make x txs; pure (M.tname x,inputNames xs,outputNames output) }))"
          , "      captures = map fst names"
          , "      captureOK = case captures of { [] -> True; first:rest -> all (== first) rest && ((first == inputName) == (workEntries == 0)) }"
          , "      observed = do { result <- M.fromPure2 make n inputs; values <- replicateM repeats (observeList left right result); pure (sum values) }"
          , "      expected = repeats * sum (map (observePure left right) (source n inputs))"
          , "      expectedTicks = toInteger (2 * width + 2 + (if left && width > 0 then workEntries else 0))"
          , "  in conjoin [counterexample (show (inputName,supplied,names)) ((captureOK && length names == width && map snd names == supplied) === True), M.correct (M.fromPure2 make n inputs) (source n inputs) === True, M.correct observed expected === True, cost observed === expectedTicks, MT.toInteger (M.infcost' (M.fromPure2 make n inputs) (listDemand left right (source n inputs))) === expectedTicks]"
          , "-- Pair ticks: one entry, then one shared work entry or one per distinct demanded expression."
          , "-- List ticks: one wrapper, width+1 map equations, width pair calls, and demanded shared work entries."
          , "-- Driver traversal/repetition and callable adapters add no source ticks."
          , "propertyFor = forAllShrink inputGen shrinkInputs $ \\(n,start,repeats,width) -> conjoin"
          , "  [conjoin (checkReturnedSharing n repeats mask : map (checkPair n repeats mask) pairFixtures ++ map (checkList n repeats width mask) listFixtures) | mask <- orderedMasks start]"
          , "boundaries = conjoin ([checkList 7 4 width mask fixture | width <- [0,3], mask <- masks, fixture <- listFixtures] ++ [checkReturnedSharing 7 4 mask | mask <- masks])"
          , "checkProperty label args property = do"
          , "  result <- quickCheckWithResult args property"
          , "  unless (isSuccess result) (print result >> fail label)"
          , "main = do"
          , "  checkProperty \"ANF empty/multi-output repeated-capture boundary\" (stdArgs { maxSuccess = 1 }) boundaries"
          , "  checkProperty \"ANF seeded sharing/value/demand/cost property\" (stdArgs { maxSuccess = 100, replay = Just (mkQCGen 20261114,0) }) propertyFor"
          , "  checkProperty \"ANF randomized sharing/value/demand/cost property\" (stdArgs { maxSuccess = 100 }) propertyFor"
          ]
      result <- compileAndRun root
      putStrLn ("PASS: anf-properties / " ++ result)

-- Keep type-specific assertions readable while exercising the same public seam.
checkThunkMatchingSharing :: IO ()
checkThunkMatchingSharing = do
    selected <- getArgs
    when (null selected || "thunk-matching-sharing" `elem` selected) $ do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-thunk-matching-sharing-"
      putStrLn ("Matching-sharing fixtures and logs: " ++ root)
      writeFile (root </> "Example.hs") $ unlines
          [ "{-# LANGUAGE ScopedTypeVariables #-}"
          , "module Example where"
          , "empty n = []"
          , "sharedEmpty = empty (1::Int)"
          , "polyCase u = case (sharedEmpty :: [Int]) of ys -> (sharedEmpty :: [Int],ys)"
          , "polyGuard u | ys <- (sharedEmpty :: [Bool]) = (sharedEmpty :: [Bool],ys)"
          , "localPolyCase u = let ys = empty u in case (ys :: [Int]) of ints -> case (ys :: [Bool]) of bools -> (ints,bools)"
          , "localPolyGuard u | let ys = empty u, ints <- (ys :: [Int]), bools <- (ys :: [Bool]) = (ints,bools)"
          , "work n = n+n"
          , "shared :: Num a => a"
          , "shared = work 5"
          , "sameCase u = case (shared :: Int) of y -> (shared :: Int,y)"
          , "sameGuard u | y <- (shared :: Int) = (shared :: Int,y)"
          , "different u | y <- (shared :: Integer) = (shared :: Int,y)"
          , "scopedCase :: forall a. Num a => a -> (a,a)"
          , "scopedCase x = case (shared :: a) of y -> (shared :: a,y)"
          , "scopedGuard :: forall a. Num a => a -> (a,a)"
          , "scopedGuard x | y <- (shared :: a) = (shared :: a,y)"
          , "localScoped :: forall a. Num a => a -> (a,a)"
          , "localScoped x = let go :: forall b. Num b => b -> (b,b); go y | z <- (shared :: b) = (shared :: b,z) in go x"
          , "wide :: Num a => (a,[b])"
          , "wide = (work 5,[])"
          , "wideBool u = case (wide :: (Int,[Bool])) of y -> (wide :: (Int,[Bool]),y)"
          , "wideInt u | y <- (wide :: (Int,[Int])) = (wide :: (Int,[Int]),y)"
          , "wideInteger u = case (wide :: (Integer,[Bool])) of y -> (wide :: (Integer,[Bool]),y)"
          , "failing :: Num a => a"
          , "failing = error \"ignored shared value evaluated\""
          , "ignoredConcrete x = case (failing :: Int) of _ -> x"
          , "ignoredScoped :: forall a. Num a => a -> a"
          , "ignoredScoped x | _ <- (failing :: a) = x"
          , "ignoredDefault x | _ <- failing = x"
          , "ignoredLocalScoped x = let go :: forall a. Num a => a -> a; go y = case (failing :: a) of _ -> y in go x"
          , "defaultedCase x = case shared of y -> if y+y > 0 then x else x"
          , "defaultedGuard x | y <- shared, y+y > 0 = x | otherwise = x"
          , "bottomCase x = let y = work (undefined :: Int) in case y of z -> (x,z)"
          , "bottomGuard x | let y = work (undefined :: Int), z <- (y :: Int) = (x,z)"
          , "computedBottomCase x = case work (undefined :: Int) of z -> (x,z)"
          , "computedBottomGuard x | z <- work (undefined :: Int) = (x,z)"
          ]
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      writeFile (root </> "Main.hs") $ unlines
          [ "module Main where"
          , "import Generated"
          , "import qualified Example as Source"
          , "import qualified Memorist.Memorist as M"
          , "import qualified Memorist.Base.Thunk as MT"
          , "import Control.Monad (unless, replicateM, replicateM_)"
          , "assert label ok = unless ok (fail label)"
          , "sameNames [] = True"
          , "sameNames (name:rest) = all (== name) rest"
          , "checkScalar :: String -> M.MemoristM Int -> Int -> Integer -> IO ()"
          , "checkScalar label program expected ticks = assert label (M.correct program expected && MT.toInteger (M.infcost program M.fullDemand) == ticks)"
          , "topPoly = do { b <- initialize; u <- M.lazy (pure ()); a <- polyCaseM b u; c <- polyGuardM b u; pure (a,c) }"
          , "topPolyDemand = do { (M.PairT a b,M.PairT c d) <- topPoly; replicateM_ 6 (M.force a >> M.force b >> M.force c >> M.force d); pure (42::Int) }"
          , "localPoly = do { b <- initialize; u <- M.lazy (pure ()); a <- localPolyCaseM b u; c <- localPolyGuardM b u; pure (a,c) }"
          , "localPolyDemand = do { (M.PairT a b,M.PairT c d) <- localPoly; replicateM_ 6 (M.force a >> M.force b >> M.force c >> M.force d); pure (42::Int) }"
          , "numeric = do"
          , "  b <- initialize"
          , "  u <- M.lazy (pure ())"
          , "  x <- M.lazy (pure (0::Int))"
          , "  y <- M.lazy (pure (0::Integer))"
          , "  a <- sameCaseM b u"
          , "  c <- sameGuardM b u"
          , "  d <- differentM b u"
          , "  e <- scopedCaseM b x"
          , "  f <- scopedGuardM b y"
          , "  g <- localScopedM b x"
          , "  pure (a,c,d,e,f,g)"
          , "numericDemand = do"
          , "  (M.PairT a b,M.PairT c d,M.PairT e f,M.PairT g h,M.PairT i j,M.PairT k l) <- numeric"
          , "  totals <- replicateM 6 $ do"
          , "    av <- M.force a; bv <- M.force b; cv <- M.force c; dv <- M.force d; ev <- M.force e; fv <- M.force f"
          , "    gv <- M.force g; hv <- M.force h; iv <- M.force i; jv <- M.force j; kv <- M.force k; lv <- M.force l"
          , "    pure (sum [av,bv,cv,dv,ev,fromInteger fv,gv,hv,fromInteger iv,fromInteger jv,kv,lv])"
          , "  pure (sum totals)"
          , "wideProgram = do { b <- initialize; u <- M.lazy (pure ()); a <- wideBoolM b u; c <- wideIntM b u; d <- wideIntegerM b u; pure (a,c,d) }"
          , "wideDemand = do"
          , "  (M.PairT a b,M.PairT c d,M.PairT e f) <- wideProgram"
          , "  totals <- replicateM 6 $ do"
          , "    M.PairT av _ <- M.force a; M.PairT bv _ <- M.force b; M.PairT cv _ <- M.force c"
          , "    M.PairT dv _ <- M.force d; M.PairT ev _ <- M.force e; M.PairT fv _ <- M.force f"
          , "    av' <- M.force av; bv' <- M.force bv; cv' <- M.force cv; dv' <- M.force dv; ev' <- M.force ev; fv' <- M.force fv"
          , "    pure (sum [av',bv',cv',dv',fromInteger ev',fromInteger fv'])"
          , "  pure (sum totals)"
          , "defaultedDemand = do { b <- initialize; a <- M.fromPure1 (defaultedCaseM b) (42::Int); c <- M.fromPure1 (defaultedGuardM b) (42::Int); pure (a+c) }"
          , "checkBottom :: String -> (Int -> (Int,Int)) -> (Bindings -> M.Th Int -> M.MemoristM (M.PairT Int Int)) -> IO ()"
          , "checkBottom label source make = do"
          , "  assert (label ++ \" source suspension\") (fst (source 42) == 42)"
          , "  let program = do { b <- initialize; M.fromPure1 (make b) (42::Int) }"
          , "      M.PairT a _ = M.value (M.eval1 program)"
          , "  assert (label ++ \" generated suspension/value/ticks\") (M.tval a == 42 && MT.toInteger (M.infcost' program (M.PairD (M.Need 42) M.Skip)) == 1)"
          , "main = do"
          , "  let (ints@(M.PairT a b),bools@(M.PairT c d)) = M.value (M.eval1 topPoly)"
          , "  assert \"top polymorphic matching identities across types\" (sameNames [M.tname a,M.tname b,M.tname c,M.tname d])"
          , "  assert \"top polymorphic source/generated values\" ((M.abstract ints :: ([Int],[Int])) == Source.polyCase () && (M.abstract bools :: ([Bool],[Bool])) == Source.polyGuard ())"
          , "  checkScalar \"top polymorphic matching: two entries plus one empty call\" topPolyDemand 42 3"
          , "  let (localA@(M.PairT la lb),localB@(M.PairT lc ld)) = M.value (M.eval1 localPoly)"
          , "  assert \"local polymorphic matching identities and evaluation freshness\" (M.tname la == M.tname lb && M.tname lc == M.tname ld && M.tname la /= M.tname lc)"
          , "  assert \"local polymorphic source/generated values\" ((M.abstract localA :: ([Int],[Bool])) == Source.localPolyCase () && (M.abstract localB :: ([Int],[Bool])) == Source.localPolyGuard ())"
          , "  checkScalar \"local polymorphic matching: two entries and two empty calls\" localPolyDemand 42 4"
          , "  let (p@(M.PairT na nb),q@(M.PairT nc nd),r@(M.PairT ne nf),s@(M.PairT ng nh),t@(M.PairT ni nj),v@(M.PairT nk nl)) = M.value (M.eval1 numeric)"
          , "  assert \"same dictionary names survive plain and scoped matching\" (sameNames [M.tname na,M.tname nb,M.tname nc,M.tname nd,M.tname ne,M.tname ng,M.tname nh,M.tname nk,M.tname nl] && sameNames [M.tname nf,M.tname ni,M.tname nj] && M.tname na /= M.tname nf)"
          , "  assert \"dictionary matching source/generated values\" ((M.abstract p :: (Int,Int)) == Source.sameCase () && (M.abstract q :: (Int,Int)) == Source.sameGuard () && (M.abstract r :: (Int,Integer)) == Source.different () && (M.abstract s :: (Int,Int)) == Source.scopedCase (0::Int) && (M.abstract t :: (Integer,Integer)) == Source.scopedGuard (0::Integer) && (M.abstract v :: (Int,Int)) == Source.localScoped (0::Int))"
          , "  checkScalar \"dictionary matching: seven source entries and two shared work calls\" numericDemand 720 9"
          , "  let (M.PairT wa wb,M.PairT wc wd,M.PairT we wf) = M.value (M.eval1 wideProgram)"
          , "  assert \"only dictionary-constrained parameters key matching identity\" (sameNames [M.tname wa,M.tname wb,M.tname wc,M.tname wd] && M.tname we == M.tname wf && M.tname wa /= M.tname we)"
          , "  assert \"wide matching source values\" (Source.wideBool () == ((10,[]),(10,[])) && Source.wideInt () == ((10,[]),(10,[])) && Source.wideInteger () == ((10,[]),(10,[])))"
          , "  checkScalar \"wide dictionary matching: three entries and two shared field computations\" wideDemand 360 5"
          , "  assert \"ignored concrete/scoped/defaulted source values\" (Source.ignoredConcrete (42::Int) == 42 && Source.ignoredScoped (42::Int) == 42 && Source.ignoredDefault (42::Int) == 42 && Source.ignoredLocalScoped (42::Int) == 42)"
          , "  checkScalar \"ignored concrete factory stays suspended\" (do { b <- initialize; M.fromPure1 (ignoredConcreteM b) (42::Int) }) 42 1"
          , "  checkScalar \"ignored scoped factory stays suspended\" (do { b <- initialize; M.fromPure1 (ignoredScopedM b) (42::Int) }) 42 1"
          , "  checkScalar \"ignored defaulted factory stays suspended\" (do { b <- initialize; M.fromPure1 (ignoredDefaultM b) (42::Int) }) 42 1"
          , "  checkScalar \"ignored locally scoped factory stays suspended\" (do { b <- initialize; M.fromPure1 (ignoredLocalScopedM b) (42::Int) }) 42 2"
          , "  assert \"demanded defaulted matching source values\" (Source.defaultedCase (42::Int) + Source.defaultedGuard (42::Int) == 84)"
          , "  checkScalar \"demanded defaulted matching shares one Integer specialization\" defaultedDemand 84 3"
          , "  checkBottom \"case local bottom\" Source.bottomCase (\\b x -> bottomCaseM b x)"
          , "  checkBottom \"annotated guard local bottom\" Source.bottomGuard (\\b x -> bottomGuardM b x)"
          , "  checkBottom \"computed case bottom\" Source.computedBottomCase (\\b x -> computedBottomCaseM b x)"
          , "  checkBottom \"computed guard bottom\" Source.computedBottomGuard (\\b x -> computedBottomGuardM b x)"
          ]
      _ <- compileAndRun root
      putStrLn "PASS: thunk-matching-sharing / polymorphic and dictionary identities / scoped and defaulted evidence / suspended bottoms / repeated-demand ticks"

checkGeneratedProperties :: IO ()
checkGeneratedProperties = do
    selected <- getArgs
    if not (null selected || "generated-properties" `elem` selected) then pure () else do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-properties-"
      writeFile (root </> "Example.hs") $ unlines
        ["module Example where", "bump x = x+1", "mapped xs = map bump xs", "add x y = x+y", "folded xs = foldr add 0 xs", "stream n = n : stream (n+1)"]
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      writeFile (root </> "Main.hs") $ unlines
        ["module Main where", "import Generated", "import qualified Example as S", "import Memorist.Memorist hiding (toInteger)",
         "import Memorist.Data.List", "import qualified Memorist.Base.Thunk as MT", "import Test.QuickCheck", "import Control.Monad (unless)",
         "propertyFor :: [Int] -> Property",
         "propertyFor xs = conjoin [ correct (fromPure1 mappedM xs) (S.mapped xs) === True, correct (fromPure1 foldedM xs) (S.folded xs) === True, MT.toInteger (infcost (fromPure1 mappedM xs) fullDemand) === (2 * toInteger (length xs) + 2), MT.toInteger (infcost (fromPure1 foldedM xs) fullDemand) === (2 * toInteger (length xs) + 2)]",
         "main = do",
         "  result <- quickCheckResult propertyFor", "  unless (isSuccess result) (fail \"generated value/cost property\")",
         "  let stream = fromPure1 streamM (5::Int)",
         "  unless (take 3 (abstract (value (eval1 stream))) == take 3 (S.stream (5::Int))) (fail \"finite infinite values\")",
         "  unless (MT.toInteger (infcost' stream (ConsD (Need 5) (Need (ConsD (Need 6) (Need (ConsD (Need 7) Skip)))))) == 3) (fail \"finite infinite cost\")"]
      result <- compileAndRun root
      putStrLn ("PASS: generated-properties / " ++ result)

checkRuntimeFailures :: IO ()
checkRuntimeFailures = do
    selected <- getArgs
    if not (null selected || "runtime-failures" `elem` selected) then pure () else do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-runtime-failures-"
      writeFile (root </> "Example.hs") $ unlines
        ["module Example where",
         "matchFirst n (x:_) | n <= 0 = 0 | otherwise = x",
         "guardFirst n _ | n <= 0 = 0", "guardFirst _ (x:_) = x",
         "demanded x = matchFirst (0::Int) undefined + x",
         "unused x = guardFirst (0::Int) undefined + x",
         "unmatched x = case ([] :: [Int]) of { y:_ -> x+y }"]
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      writeFile (root </> "Main.hs") $ unlines
        ["module Main where", "import Generated", "import qualified Example as S",
         "import Memorist.Memorist", "import qualified Memorist.Base.Thunk as MT",
         "import Control.Exception", "import Control.Monad (unless)",
         "mustFail label x = do { outcome <- try (evaluate x) :: IO (Either SomeException Bool); case outcome of { Left _ -> pure (); Right _ -> fail label } }",
         "main = do",
         "  mustFail \"source demanded failure\" (S.demanded (5::Int) == 5)",
         "  mustFail \"generated demanded failure\" (correct (fromPure1 demandedM (5::Int)) (5::Int))",
         "  mustFail \"source unmatched failure\" (S.unmatched (5::Int) == 5)",
         "  mustFail \"generated unmatched failure\" (correct (fromPure1 unmatchedM (5::Int)) (5::Int))",
         "  unless (S.unused (5::Int) == 5 && correct (fromPure1 unusedM (5::Int)) (5::Int)) (fail \"guard-first value\")",
         "  unless (MT.toInteger (infcost (fromPure1 unusedM (5::Int)) fullDemand) == 2) (fail \"guard-first cost\")"]
      _ <- compileAndRun root
      putStrLn "PASS: source/generated demanded failures and guard-before-pattern behavior"

checkDiagnostics :: IO ()
checkDiagnostics = do
    selected <- getArgs
    if not (null selected || "diagnostics" `elem` selected) then pure () else do
      mapM_ (\(label, program, message) -> do
        tmp <- getTemporaryDirectory
        root <- createTempDirectory tmp "cost-translate-diagnostic-"
        let source = root </> "Example.hs"; output = root </> "Generated.hs"
        writeFile source program
        (status, out, err) <- readProcessWithExitCode "cost-translate" [source,output] ""
        published <- doesFileExist output
        logText <- readFile (output ++ ".log")
        unless (status /= ExitSuccess && not published && message `isInfixOf` (out ++ err ++ logText) && "Example.hs" `isInfixOf` logText) $
            fail (label ++ ": wrong diagnostic/publication: " ++ show (status,out,err,logText))
        when ("private-" `isPrefixOf` label) $
            unless ("Example.hs:2:" `isInfixOf` logText && not ("Failed candidate:" `isInfixOf` logText)) $
                fail "private constructor must be rejected at its declaration before generated typechecking"
        when ("pattern-bind-" `isPrefixOf` label) $
            unless ("Example.hs:2:" `isInfixOf` logText && "(x, y)" `isInfixOf` logText) $
                fail ("pattern binding diagnostic must include its location and pattern: " ++ logText)
        when ("local-signature-" `isPrefixOf` label) $
            unless (any (\l -> "Example.hs:2:" `isInfixOf` l && message `isInfixOf` l) (lines logText)) $
                fail ("local signature diagnostic must include its source location: " ++ logText)
        putStrLn ("PASS: CLI " ++ label))
        [("syntax", "module Example where\nf x =\n", "source error"),
         ("type", "module Example where\nf :: Int -> Bool\nf x = x\n", "source error"),
         ("lambda", "module Example where\nf x = (\\y -> y) x\n", "unsupported"),
         ("strict-semantics", "{-# LANGUAGE Strict #-}\nmodule Example where\nf x = x\n", "Strict"),
         ("recursive-value", "module Example where\nx = x\nf y = y\n", "recursive value"),
         ("deriving", "module Example where\ndata Box = Box Int deriving Eq\nf x = x\n", "deriving"),
         ("function-alias", "module Example where\nadd x y = x+y\nf x = let alias = add x in x\n", "alias"),
         ("strict-field", "module Example where\ndata Box = Box !Int\nf x = x\n", "strict field"),
         ("structured-comparison", "module Example where\nf x = if [x] == [x] then x else 0\n", "strict scalar"),
         ("private-constructor-conversion", "module Example (Box, make, readBox, observed) where\ndata Box = Box Int\nmake x = Box x\nreadBox (Box x) = x\nobserved x = readBox (make x)\n", "export constructor Box"),
         ("private-second-constructor", "module Example (Choice(First), f) where\ndata Choice = First Int | Second Int\nf x = x\n", "export constructor Second"),
         ("private-datatype", "module Example (f) where\ndata Hidden = Hidden Int\nf x = x\n", "export constructor Hidden"),
         ("pattern-bind-top", "module Example where\n(x,y) = (1::Int, 2::Int)\nf n = n\n", "unsupported pattern binding"),
         ("pattern-bind-local", "module Example where\nf n = let (x,y) = (1::Int, 2::Int) in n\n", "unsupported pattern binding"),
         ("local-signature-constraint", "module Example where\nf x = let g :: Monoid a => a -> a; g y = y in x\n", "unsupported type constraint")]
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-missing-runtime-"
      writeFile (root </> "Example.hs") "module Example where\nf x = x\n"
      (status, out, err) <- readProcessWithExitCode "/usr/bin/env"
          ["-u", "GHC_PACKAGE_PATH", "-u", "GHC_ENVIRONMENT", "cost-translate", root </> "Example.hs", root </> "Generated.hs"] ""
      published <- doesFileExist (root </> "Generated.hs")
      unless (status /= ExitSuccess && not published && "setup error" `isInfixOf` (out ++ err)) (fail ("missing runtime diagnostic: " ++ show (status,out,err)))
      putStrLn "PASS: CLI missing runtime is a setup error"

checkDemands :: IO ()
checkDemands = do
    selected <- getArgs
    if not (null selected || "datatypes-and-demands" `elem` selected) then pure () else do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-demands-"
      putStrLn ("Demand fixtures and logs: " ++ root)
      writeFile (root </> "Example.hs") $ unlines
        ["module Example where", "data Tree a = Leaf | Node a (Tree a) (Tree a)",
         "data Nested a = End a | More (Nested [a])", "grow x = Node x (grow x) (grow x)"]
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      writeFile (root </> "Main.hs") $ unlines
        ["module Main where", "import Generated", "import qualified Example as S",
         "import Memorist.Memorist", "import qualified Memorist.Base.Thunk as MT",
         "import Test.QuickCheck", "import Control.Monad (unless, replicateM_)",
         "import System.Timeout (timeout)",
         "program = fromPure1 growM (5::Int)",
         "tree = value (eval1 program)",
         "nodes LeafD = 1", "nodes (NodeD a l r) = 1 + field a + branch l + branch r",
         "field Skip = 0", "field (Need _) = 1",
         "branch Skip = 0", "branch (Need d) = 1 + nodes d",
         "compatible LeafT LeafD = True",
         "compatible (NodeT a l r) (NodeD da dl dr) = scalar da && child l dl && child r dr",
         "compatible _ _ = False", "scalar Skip = True", "scalar (Need n) = n == (5::Int)",
         "child _ Skip = True", "child t (Need d) = compatible (tval t) d",
         "main = do",
         "  unless (MT.toInteger (infcost' program (NodeD (Need 5) Skip Skip)) == 1) (fail \"explicit finite demand cost\")",
         "  result <- timeout 10000000 $ replicateM_ 100 $ do",
         "    d <- generate (genTreeDemand 15 tree)",
         "    unless (nodes d <= 16) (fail \"budget exceeded\")",
         "    unless (compatible tree d && all (compatible tree) (shrinkTreeDemand tree d)) (fail \"incompatible demand/shrink\")",
         "    mapM_ (\\s -> print (MT.toInteger (infcost' program s))) (shrinkTreeDemand tree d)",
         "  case result of { Nothing -> fail \"infinite demand generation\"; Just () -> pure () }",
         "  let nested = value (eval1 (reprM (S.More (S.End [1,2::Int])) :: MemoristM (NestedT Int)))",
         "  d <- generate (genNestedDemand 10 nested)",
         "  mapM_ print (shrinkNestedDemand nested d)",
         "  custom <- generate (pure (NodeD Skip Skip Skip))",
         "  unless (MT.toInteger (infcost' program custom) == 1) (fail \"custom demand\")",
         "  putStrLn \"PASS: datatype conversion, nested parameters, bounded compatible demands, shrinking and customization\""]
      _ <- compileAndRun root
      putStrLn "PASS: datatypes-and-demands"

checkWholeModuleFailure :: IO ()
checkWholeModuleFailure = do
    tmp <- getTemporaryDirectory
    root <- createTempDirectory tmp "cost-translate-failure-"
    putStrLn ("Failure fixtures and logs: " ++ root)
    let source = root </> "Example.hs"
        output = root </> "Generated.hs"
    writeFile source "module Example where\nnewtype Box = Box Int\nf x = x\n"
    result <- translateFile source output
    published <- doesFileExist output
    logText <- readFile (output ++ ".log")
    unless (either (const True) (const False) result && not published && "arity" `isInfixOf` logText) $
        fail "unsupported newtype must fail the whole module and retain the arity log"
    putStrLn "PASS: whole-module failure / no publication / retained arity log"

runExample :: String -> String -> String -> Int -> Integer -> IO ()
runExample label program function = runAction label program
    ("Source." ++ init function ++ " (5 :: Int)")
    ("fromPure1 " ++ function ++ " (5 :: Int)")

runAction :: String -> String -> String -> String -> Int -> Integer -> IO ()
runAction label program sourceExpr action expected cost = do
    selected <- getArgs
    when (null selected || label `elem` selected) run
  where
   run = do
    tmp <- getTemporaryDirectory
    root <- createTempDirectory tmp "cost-translate-e2e-"
    putStrLn ("Fixtures and logs: " ++ root)
    let source = root </> "Example.hs"
        output = root </> "Generated.hs"
    writeFile source (if any (`isPrefixOf` program) ["module ", "{-# LANGUAGE"] then program else "module Example where\n" ++ program)
    translated <- translateFile source output
    either (fail . ("translation failed: " ++)) pure translated
    when (label == "foldr-arity-four-partial") $ do
        logText <- readFile (output ++ ".log")
        unless ("Example.add4: supplied 2 / arity 4; partial adapter" `isInfixOf` logText &&
                not ("Example.add4: supplied 1" `isInfixOf` logText) &&
                not ("foldr: supplied 1" `isInfixOf` logText)) $
            fail "arity log must describe maximal applications, not intermediate AST spines"
    writeFile (root </> "Main.hs") $ unlines
        [ "module Main where"
        , "import Generated"
        , "import qualified Example as Source"
        , "import Memorist.Memorist"
        , "import qualified Memorist.Base.Thunk as MT"
        , "translated :: MemoristM Int"
        , "translated = " ++ action
        , "main = do"
        , "  if (" ++ sourceExpr ++ ") == (" ++ show expected ++ " :: Int) then pure () else error \"source oracle failed\""
        , "  print (correct translated (" ++ show expected ++ " :: Int),"
        , "    MT.toInteger (infcost translated fullDemand))"
        ]
    result <- compileAndRun root
    unless (result == "(True," ++ show cost ++ ")\n") $
        fail (label ++ ": expected value " ++ show expected ++ " and cost " ++ show cost
              ++ ", got " ++ show result)
    putStrLn ("PASS: " ++ label ++ " / generated typecheck / value " ++ show expected ++ " / cost " ++ show cost)
