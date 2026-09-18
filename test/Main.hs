module Main (main) where

import Cost.Translate (translateFile)
import Control.Monad (unless, when)
import Control.Exception (SomeException, displayException, try)
import System.Directory (getTemporaryDirectory, doesFileExist)
import Data.List (isInfixOf, isPrefixOf)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import System.Process (readProcessWithExitCode)
import System.Environment (getArgs)

-- Deliberately retain fixtures and logs on failure for diagnosis.
main :: IO ()
main = do
    runExample "double"
        "double :: Int -> Int\ndouble x = x + x\n"
        "doubleM" 10 1
    runExample "shared-local"
        "double x = x + x\nshared x = let y = double x in y + y\n"
        "sharedM" 20 2
    runExample "unused-failing-control-flow"
        "keep x = let unused = if (undefined :: Bool) then x else x in x\n"
        "keepM" 5 1
    runExample "captured-local-function"
        "double x = x + x\ncaptured x = let y = double x; add z = y + z in add x + add x\n"
        "capturedM" 30 4
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
      writeFile (root </> "Example.hs") "module Example where\ndata One = One { only :: Int }\nidentity x = x\nunbox (One x) = x\n"
      result <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure result
      (compiled,out,err) <- readProcessWithExitCode "ghc"
          ["-v0", "-fno-code", "-fforce-recomp", "-Werror=unused-local-binds", "-Werror=overlapping-patterns", "-Werror=simplifiable-class-constraints",
           "-package", "MemoristANT", "-package", "QuickCheck", "-i" ++ root, root </> "Generated.hs"] ""
      writeFile (root </> "warnings.log") (out ++ err)
      unless (compiled == ExitSuccess) (fail err)
      putStrLn "PASS: generated code has no unused fallbacks or redundant single-constructor cases"

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
