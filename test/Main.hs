module Main (main) where

import Cost.Translate (translateFile)
import Control.Monad (unless, when)
import Control.Exception (SomeException, displayException, try)
import System.Directory (copyFile, getTemporaryDirectory, doesFileExist, findExecutable, listDirectory)
import Data.List (intercalate, isInfixOf, isPrefixOf, nub, sort)
import Data.Char (isSpace)
import FixturePolicy (checkFixturePolicy)
import Paths_cost_translate (getDataFileName)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (createTempDirectory, withSystemTempDirectory)
import System.Process (CreateProcess(env), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import System.Environment (getArgs, getEnvironment, getExecutablePath)

-- Deliberately retain fixtures and logs on failure for diagnosis.
main :: IO ()
main = do
    selected <- getArgs
    let names = concatMap groupNames testRegistry
        unknown = nub (filter (`notElem` names) selected)
    unless (null unknown) $ do
        hPutStrLn stderr ("Unknown test selector(s): " ++ intercalate ", " (map show unknown))
        hPutStrLn stderr "Usage: cost-translate-test [SELECTOR ...] (no selectors runs all groups)"
        hPutStrLn stderr ("Available test selectors: " ++ intercalate ", " names)
        exitFailure
    mapM_ (runGroup (\name -> null selected || name `elem` selected)) testRegistry

data TestGroup
    = TestGroup String (IO ())
    | TestAudit [(String, IO ())]

groupNames :: TestGroup -> [String]
groupNames (TestGroup name _) = [name]
groupNames (TestAudit tests) = map fst tests

runGroup :: (String -> Bool) -> TestGroup -> IO ()
runGroup selected (TestGroup name action) = when (selected name) action
runGroup selected (TestAudit tests) = checkHigherKindedAudit (filter (selected . fst) tests)

-- Registration order is the full-suite order; names and actions stay together.
testRegistry :: [TestGroup]
testRegistry =
    [ TestGroup "thunk-forwarding" checkThunkForwarding
    , TestGroup "thunk-case" (checkThunkMatching "thunk-case")
    , TestGroup "thunk-guards" (checkThunkMatching "thunk-guards")
    , TestGroup "anf-demand" checkAnfDemand
    , TestGroup "thunk-fields" checkThunkFields
    , TestGroup "thunk-captures" checkThunkCaptures
    , TestGroup "thunk-sharing" checkThunkSharing
    , TestGroup "thunk-matching-sharing" checkThunkMatchingSharing
    , TestGroup "anf-properties" checkAnfProperties
    , TestGroup "anf-scoped-renaming" checkAnfScopedRenaming
    , example "scoped-case-alias-shadow" 1 3
    , example "scoped-guard-alias-shadow" 1 3
    , action "scoped-factory-case-shadow-int" "scoped-factory-case-shadow/Example.hs" 1 3
    , action "scoped-factory-case-shadow-integer" "scoped-factory-case-shadow/Example.hs" 2 4
    , action "scoped-factory-guard-shadow-int" "scoped-factory-guard-shadow/Example.hs" 1 3
    , action "scoped-factory-guard-shadow-integer" "scoped-factory-guard-shadow/Example.hs" 2 4
    , example "double" 10 1
    , example "shared-local" 20 2
    , example "shared-forwarded-arguments" 20 3
    , example "unused-failing-control-flow" 5 1
    , example "captured-local-function" 30 4
    , example "readable-name-argument-collision" 5 2
    , example "readable-name-global-collision" 12 3
    , example "readable-name-field-collision" 8 2
    , example "readable-name-anchor-collision" 6 1
    , example "readable-name-case-thunk-collision" 6 2
    , example "readable-name-continuation-collisions" 5 2
    , example "readable-name-guard-thunk-collision" 5 1
    , example "readable-parenthesized-parameter" 5 1
    , TestGroup "whole-module-failure" checkWholeModuleFailure
    , example "handwritten-filter-lazy-tail" 5 4
    , example "ordered-equations-and-conjunctive-guards" 5 2
    , example "finite-observation-of-recursive-producer" 6 4
    , example "ternary-partials-capture-and-composition" 25 6
    , example "shared-top-level-across-exported-calls" 30 3
    , example "scanl-unused-input" 5 3
    , example "lazy-datatype-primitive-field" 5 3
    , TestGroup "datatypes-and-demands" checkDemands
    , example "foldr-arity-four-partial" 30 6
    , example "foldl-named" 10 6
    , example "zipWith-named" 10 4
    , example "concatMap-named" 5 6
    , example "repeatly-external-identity" 10 4
    , example "mutually-recursive-captured-helpers" 5 6
    , example "case-pattern-and-let-guards" 10 1
    , example "literal-pattern-fallback" 5 2
    , example "char-scalar-field" 5 2
    , example "literal-pattern-char-string" 14 4
    , example "direct-composition-application" 20 3
    , example "polymorphic-shared-top-level-value" 5 3
    , example "polymorphic-local-value" 10 4
    , example "resolved-user-map" 10 2
    , example "unused-error-and-short-circuit" 5 1
    , example "record-lazy-fields-and-selectors" 10 3
    , example "nested-pair-and-maybe-fields" 5 3
    , example "nested-case-retains-outer-binding" 15 1
    , example "named-source-operator" 10 2
    , example "infix-constructor" 5 2
    , TestGroup "diagnostics" checkDiagnostics
    , TestGroup "runtime-failures" checkRuntimeFailures
    , TestGroup "generated-properties" checkGeneratedProperties
    , example "direct-foldr-maybe" (-5) 3
    , example "direct-foldl-pair-unused-first-field" 5 3
    , example "direct-foldr-nothing-unused-callback" 5 2
    , TestGroup "pattern-scaling" checkPatternScaling
    , example "scalar-division-and-negation" 6 1
    , example "direct-concatMap-maybe" 5 7
    , example "explicit-constructor-exports" 5 3
    , TestAudit
        [ (label, runExample label 5 3)
        | label <- ["higher-kinded-datatype-field", "higher-kinded-recursive-field", "higher-kinded-polykinded-field"]
        ]
    , TestGroup "higher-kinded-demands" checkHigherKindedDemands
    , example "datatype-name-shadows-foldable-class" 5 2
    , example "constrained-sharing-different-dictionaries" 12 5
    , action "constrained-sharing-same-dictionary" "constrained-sharing-different-dictionaries/Example.hs" 2 3
    , example "constrained-sharing-unconstrained-parameter" 2 3
    , example "constrained-sharing-local" 5 6
    , example "constrained-sharing-generic-caller" 12 7
    , example "constrained-sharing-ignored-signature" 5 2
    , example "constrained-sharing-scoped-signature" 5 2
    , example "constrained-sharing-local-scoped-signature" 5 3
    , example "constrained-sharing-defaulted-use" 5 2
    , example "constrained-sharing-constructor-dictionaries" 12 4
    , example "constrained-sharing-all-dictionaries" 20 4
    , example "constrained-sharing-nested-allocations" 12 7
    , example "injective-value-names" 16 3
    , example "injective-constructor-names" 15 3
    , example "injective-type-and-helper-names" 10 3
    , example "identifier-escaping-magic-hash" 5 2
    , example "record-selector-prelude-collision" 5 2
    , example "record-pattern-source-order" 2 2
    , TestGroup "generated-warnings" checkGeneratedWarnings
    , TestGroup "generated-module-name-diagnostic" checkGeneratedModuleNameDiagnostic
    , example "composition-partial-map" 30 12
    , example "composition-prefix" 20 5
    , example "composition-generalized-partial" 40 7
    , example "composition-nested-partial-sharing" 50 13
    , example "composition-captured-operands" 70 16
    , example "composition-arity-two-right-operand" 10 6
    , TestGroup "test-selection" checkTestSelection
    , TestGroup "fixture-policy" checkFixturePolicy
    ]
  where
    example label expected cost = TestGroup label (runExample label expected cost)
    action label source expected cost = TestGroup label (runAction label source expected cost)

checkTestSelection :: IO ()
checkTestSelection = do
    executable <- getExecutablePath
    let unknown = "__retro_unknown_selector__"
        anotherUnknown = "__retro_another_unknown_selector__"
        wholeModulePass = "PASS: whole-module failure / no publication / retained arity log"
        moduleNamePass = "PASS: invalid generated module names retain their category and candidate"
    checkRejected executable [unknown] [unknown]
    checkRejected executable
        ["whole-module-failure", unknown, "generated-module-name-diagnostic", anotherUnknown]
        [unknown, anotherUnknown]
    checkAccepted executable ["whole-module-failure"] [wholeModulePass]
    checkAccepted executable ["generated-module-name-diagnostic"] [moduleNamePass]
    checkAccepted executable
        ["generated-module-name-diagnostic", "whole-module-failure"]
        [wholeModulePass, moduleNamePass]
    checkAccepted executable
        ["generated-module-name-diagnostic", "whole-module-failure", "generated-module-name-diagnostic", "whole-module-failure"]
        [wholeModulePass, moduleNamePass]
    putStrLn "PASS: test-selection / unknown and mixed selectors rejected / requested groups run once in registry order / no unrelated groups"
  where
    checkRejected executable selectors unknowns =
      withSystemTempDirectory "cost-translate-test-selection-" $ \tmp -> do
        environment <- getEnvironment
        let child = (proc executable selectors)
                { env = Just (("TMPDIR", tmp) : filter ((/= "TMPDIR") . fst) environment) }
        (status, out, err) <- readCreateProcessWithExitCode child ""
        unless (status /= ExitSuccess && null out &&
                "Unknown test selector(s):" `isPrefixOf` err && all (`isInfixOf` err) unknowns &&
                "Usage: cost-translate-test" `isInfixOf` err && "Available test selectors:" `isInfixOf` err) $
            fail ("unknown selectors must fail before running groups\n" ++ childResult selectors status out err)
        retained <- listDirectory tmp
        unless (null retained) $
            fail ("unknown selectors created retained test files: " ++ show (selectors, retained))
    checkAccepted executable selectors expectedPasses = do
        (status, out, err) <- readProcessWithExitCode executable selectors ""
        unless (status == ExitSuccess && filter ("PASS: " `isPrefixOf`) (lines out) == expectedPasses) $
            fail ("requested groups did not run exactly once in registry order\n" ++
                  "expected PASS lines: " ++ show expectedPasses ++ "\n" ++ childResult selectors status out err)
    childResult selectors status out err = unlines
        [ "selectors: " ++ show selectors
        , "exit status: " ++ show status
        , "stdout: " ++ show out
        , "stderr: " ++ show err
        ]

checkThunkForwarding :: IO ()
checkThunkForwarding = do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-thunk-forwarding-"
      putStrLn ("Thunk-forwarding fixtures and logs: " ++ root)
      copyFixture "thunk-forwarding/Example.hs" (root </> "Example.hs")
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      copyFixture "thunk-forwarding/Main.hs" (root </> "Main.hs")
      _ <- compileAndRun root
      putStrLn "PASS: thunk-forwarding / unused argument / two ticks / no forwarding allocation"

checkThunkMatching :: String -> IO ()
checkThunkMatching label = do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp ("cost-translate-" ++ label ++ "-")
      putStrLn ("Thunk-matching fixtures and logs: " ++ root)
      copyFixture "thunk-matching/Example.hs" (root </> "Example.hs")
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      copyFixture (label </> "Main.hs") (root </> "Main.hs")
      _ <- compileAndRun root
      putStrLn ("PASS: " ++ label ++ " / match alias identities / no forwarding allocation / lazy subjects / entry ticks")

checkAnfDemand :: IO ()
checkAnfDemand = do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-anf-demand-"
      putStrLn ("ANF-demand fixtures and logs: " ++ root)
      copyFixture "anf-demand/Example.hs" (root </> "Example.hs")
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      copyFixture "anf-demand/Main.hs" (root </> "Main.hs")
      _ <- compileAndRun root
      putStrLn "PASS: anf-demand / fresh suspension, explicit anchors, ordered matching, scalar caller allocations, scalar and Boolean demand"

checkThunkFields :: IO ()
checkThunkFields = do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-thunk-fields-"
      putStrLn ("Thunk-field fixtures and logs: " ++ root)
      copyFixture "thunk-fields/Example.hs" (root </> "Example.hs")
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      copyFixture "thunk-fields/Main.hs" (root </> "Main.hs")
      _ <- compileAndRun root
      putStrLn "PASS: thunk-fields / constructor, record, tuple, list identities / suspended computed fields"

checkThunkCaptures :: IO ()
checkThunkCaptures = do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-thunk-captures-"
      putStrLn ("Thunk-capture fixtures and logs: " ++ root)
      copyFixture "thunk-captures/Example.hs" (root </> "Example.hs")
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      copyFixture "thunk-captures/Main.hs" (root </> "Main.hs")
      _ <- compileAndRun root
      putStrLn "PASS: thunk-captures / partial and composition captures / retained composition intermediate"

checkThunkSharing :: IO ()
checkThunkSharing = do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-thunk-sharing-"
      putStrLn ("Thunk-sharing fixtures and logs: " ++ root)
      copyFixture "thunk-sharing/Example.hs" (root </> "Example.hs")
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      copyFixture "thunk-sharing/Main.hs" (root </> "Main.hs")
      _ <- compileAndRun root
      putStrLn "PASS: thunk-sharing / local and dictionary-sensitive thunk identities / at-most-once ticks"

checkGeneratedModuleNameDiagnostic :: IO ()
checkGeneratedModuleNameDiagnostic = do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-generated-parse-"
      let source = root </> "Example.hs"; output = root </> "bad-name.hs"
      copyFixture "identity/Example.hs" source
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
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-warnings-"
      copyFixture "generated-warnings/Example.hs" (root </> "Example.hs")
      result <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure result
      copyFixture "generated-warnings/Minimal.hs" (root </> "Minimal.hs")
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
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-higher-demands-"
      putStrLn ("Higher-kinded fixtures and logs: " ++ root)
      copyFixture "higher-kinded-demands/Example.hs" (root </> "Example.hs")
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      copyFixture "higher-kinded-demands/Main.hs" (root </> "Main.hs")
      _ <- compileAndRun root
      putStrLn "PASS: higher-kinded conversion, custom constructors, index kinds, infinite bounded demands and shrinking"

-- Keep all three original regressions live; report every failure if they recur.
checkHigherKindedAudit :: [(String, IO ())] -> IO ()
checkHigherKindedAudit tests = do
    failures <- mapM check tests
    unless (all null failures) (fail (unlines (filter (not . null) failures)))
  where
    check (label, action) = do
        result <- try action :: IO (Either SomeException ())
        pure (either (\e -> label ++ ": " ++ displayException e) (const "") result)

checkPatternScaling :: IO ()
checkPatternScaling = do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-patterns-"
      copyFixture "pattern-scaling/Example.hs" (root </> "Example.hs")
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      generated <- readFile (root </> "Generated.hs")
      unless (length generated < 40000) (fail ("fallback duplication: " ++ show (length generated) ++ " bytes"))
      copyFixture "pattern-scaling/Main.hs" (root </> "Main.hs")
      _ <- compileAndRun root
      putStrLn ("PASS: pattern-scaling / 169 values and costs / " ++ show (length generated) ++ " bytes")

copyFixture :: FilePath -> FilePath -> IO ()
copyFixture fixture destination = do
    source <- getDataFileName ("test" </> "cases" </> fixture)
    copyFile source destination

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
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-anf-properties-"
      putStrLn ("ANF property fixtures and logs: " ++ root)
      copyFixture "anf-properties/Example.hs" (root </> "Example.hs")
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      copyFixture "anf-properties/Main.hs" (root </> "Main.hs")
      result <- compileAndRun root
      putStrLn ("PASS: anf-properties / " ++ result)

-- Compile paired programs separately: generated instances cannot coexist in one
-- driver. Each variant has its own source/value/tick oracle before comparison.
checkAnfScopedRenaming :: IO ()
checkAnfScopedRenaming = do
    shadowedPath <- getDataFileName ("test" </> "cases" </> "anf-scoped-renaming" </> "Shadowed.hs")
    renamedPath <- getDataFileName ("test" </> "cases" </> "anf-scoped-renaming" </> "Renamed.hs")
    shadowedSource <- readFile shadowedPath
    renamedSource <- readFile renamedPath
    unless (length (filter (isInfixOf shadowedSignature) (lines shadowedSource)) == 4 &&
            renameInner shadowedSource == renamedSource) $
        fail "scoped-renaming fixtures must differ only in the four inner forall binders"
    tmp <- getTemporaryDirectory
    root <- createTempDirectory tmp "cost-translate-anf-scoped-renaming-"
    putStrLn ("ANF scoped-renaming fixtures and logs: " ++ root)
    renamed <- runVariant root "renamed" "Renamed.hs"
    shadowed <- runVariant root "shadowed" "Shadowed.hs"
    unless (shadowed == renamed) $
        fail ("renaming inner type variables changed observations\nshadowed: " ++ shadowed ++ "\nrenamed: " ++ renamed)
    putStrLn "PASS: anf-scoped-renaming / paired case and guard shapes / parameter and factory specializations / values and entry ticks"
  where
    shadowedSignature = "inner :: forall a. a -> Int"
    renamedSignature = "inner :: forall b. b -> Int"
    renameInner [] = []
    renameInner input@(c:rest)
        | shadowedSignature `isPrefixOf` input = renamedSignature ++ renameInner (drop (length shadowedSignature) input)
        | otherwise = c : renameInner rest
    runVariant root label fixture = do
        variant <- createTempDirectory root (label ++ "-")
        copyFixture ("anf-scoped-renaming" </> fixture) (variant </> "Example.hs")
        translated <- translateFile (variant </> "Example.hs") (variant </> "Generated.hs")
        either (fail . ((label ++ " scoped-renaming translation failed: ") ++)) pure translated
        copyFixture ("anf-scoped-renaming" </> "Main.hs") (variant </> "Main.hs")
        result <- compileAndRun variant
        putStrLn ("PASS: anf-scoped-renaming / " ++ label ++ " source/generated values and entry ticks")
        pure result

-- Keep type-specific assertions readable while exercising the same public seam.
checkThunkMatchingSharing :: IO ()
checkThunkMatchingSharing = do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-thunk-matching-sharing-"
      putStrLn ("Matching-sharing fixtures and logs: " ++ root)
      copyFixture "thunk-matching-sharing/Example.hs" (root </> "Example.hs")
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      copyFixture "thunk-matching-sharing/Main.hs" (root </> "Main.hs")
      _ <- compileAndRun root
      putStrLn "PASS: thunk-matching-sharing / polymorphic and dictionary identities / scoped and defaulted evidence / suspended bottoms / repeated-demand ticks"

checkGeneratedProperties :: IO ()
checkGeneratedProperties = do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-properties-"
      copyFixture "generated-properties/Example.hs" (root </> "Example.hs")
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      copyFixture "generated-properties/Main.hs" (root </> "Main.hs")
      result <- compileAndRun root
      putStrLn ("PASS: generated-properties / " ++ result)

checkRuntimeFailures :: IO ()
checkRuntimeFailures = do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-runtime-failures-"
      copyFixture "runtime-failures/Example.hs" (root </> "Example.hs")
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      copyFixture "runtime-failures/Main.hs" (root </> "Main.hs")
      _ <- compileAndRun root
      putStrLn "PASS: source/generated demanded failures and guard-before-pattern behavior"

checkDiagnostics :: IO ()
checkDiagnostics = do
      mapM_ (\(label, message) -> do
        tmp <- getTemporaryDirectory
        root <- createTempDirectory tmp "cost-translate-diagnostic-"
        let source = root </> "Example.hs"; output = root </> "Generated.hs"
        copyFixture (("diagnostics-" ++ label) </> "Example.hs") source
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
        [("syntax", "source error"),
         ("type", "source error"),
         ("lambda", "unsupported"),
         ("strict-semantics", "Strict"),
         ("recursive-value", "recursive value"),
         ("deriving", "deriving"),
         ("function-alias", "alias"),
         ("strict-field", "strict field"),
         ("structured-comparison", "strict scalar"),
         ("private-constructor-conversion", "export constructor Box"),
         ("private-second-constructor", "export constructor Second"),
         ("private-datatype", "export constructor Hidden"),
         ("pattern-bind-top", "unsupported pattern binding"),
         ("pattern-bind-local", "unsupported pattern binding"),
         ("local-signature-constraint", "unsupported type constraint")]
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-missing-runtime-"
      copyFixture "identity/Example.hs" (root </> "Example.hs")
      (status, out, err) <- readProcessWithExitCode "/usr/bin/env"
          ["-u", "GHC_PACKAGE_PATH", "-u", "GHC_ENVIRONMENT", "cost-translate", root </> "Example.hs", root </> "Generated.hs"] ""
      published <- doesFileExist (root </> "Generated.hs")
      unless (status /= ExitSuccess && not published && "setup error" `isInfixOf` (out ++ err)) (fail ("missing runtime diagnostic: " ++ show (status,out,err)))
      putStrLn "PASS: CLI missing runtime is a setup error"

checkDemands :: IO ()
checkDemands = do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-demands-"
      putStrLn ("Demand fixtures and logs: " ++ root)
      copyFixture "datatypes-and-demands/Example.hs" (root </> "Example.hs")
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      copyFixture "datatypes-and-demands/Main.hs" (root </> "Main.hs")
      _ <- compileAndRun root
      putStrLn "PASS: datatypes-and-demands"

checkWholeModuleFailure :: IO ()
checkWholeModuleFailure = do
    tmp <- getTemporaryDirectory
    root <- createTempDirectory tmp "cost-translate-failure-"
    putStrLn ("Failure fixtures and logs: " ++ root)
    let source = root </> "Example.hs"
        output = root </> "Generated.hs"
    copyFixture "whole-module-failure/Example.hs" source
    result <- translateFile source output
    published <- doesFileExist output
    logText <- readFile (output ++ ".log")
    unless (either (const True) (const False) result && not published && "arity" `isInfixOf` logText) $
        fail "unsupported newtype must fail the whole module and retain the arity log"
    putStrLn "PASS: whole-module failure / no publication / retained arity log"

runExample :: String -> Int -> Integer -> IO ()
runExample label = runAction label (label </> "Example.hs")

runAction :: String -> FilePath -> Int -> Integer -> IO ()
runAction label sourceFixture expected cost = do
    tmp <- getTemporaryDirectory
    root <- createTempDirectory tmp "cost-translate-e2e-"
    putStrLn ("Fixtures and logs: " ++ root)
    let source = root </> "Example.hs"
        output = root </> "Generated.hs"
    copyFixture sourceFixture source
    translated <- translateFile source output
    either (fail . ("translation failed: " ++)) pure translated
    when (label == "foldr-arity-four-partial") $ do
        logText <- readFile (output ++ ".log")
        unless ("Example.add4: supplied 2 / arity 4; partial adapter" `isInfixOf` logText &&
                not ("Example.add4: supplied 1" `isInfixOf` logText) &&
                not ("foldr: supplied 1" `isInfixOf` logText)) $
            fail "arity log must describe maximal applications, not intermediate AST spines"
    copyFixture (label </> "Main.hs") (root </> "Main.hs")
    result <- compileAndRun root
    unless (result == "(True," ++ show cost ++ ")\n") $
        fail (label ++ ": expected value " ++ show expected ++ " and cost " ++ show cost
              ++ ", got " ++ show result)
    putStrLn ("PASS: " ++ label ++ " / generated typecheck / value " ++ show expected ++ " / cost " ++ show cost)
