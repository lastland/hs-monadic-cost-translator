module Main (main) where

import Cost.Translate (translateFile)
import Control.Monad (unless, when)
import Control.Exception (SomeException, displayException, try)
import System.Directory (copyFile, getTemporaryDirectory, doesFileExist, findExecutable)
import Data.List (isInfixOf, isPrefixOf, sort)
import Data.Char (isSpace)
import Paths_cost_translate (getDataFileName)
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
    runExample "scoped-case-alias-shadow" 1 3
    runExample "scoped-guard-alias-shadow" 1 3
    mapM_ (\label -> do
        runAction (label ++ "-int") (label </> "Example.hs") 1 3
        runAction (label ++ "-integer") (label </> "Example.hs") 2 4)
        ["scoped-factory-case-shadow", "scoped-factory-guard-shadow"]
    runExample "double" 10 1
    runExample "shared-local" 20 2
    runExample "shared-forwarded-arguments" 20 3
    runExample "unused-failing-control-flow" 5 1
    runExample "captured-local-function" 30 4
    runExample "readable-name-argument-collision" 5 2
    runExample "readable-name-global-collision" 12 3
    runExample "readable-name-field-collision" 8 2
    runExample "readable-name-anchor-collision" 6 1
    runExample "readable-name-case-thunk-collision" 6 2
    runExample "readable-name-continuation-collisions" 5 2
    runExample "readable-name-guard-thunk-collision" 5 1
    runExample "readable-parenthesized-parameter" 5 1
    checkWholeModuleFailure
    runExample "handwritten-filter-lazy-tail" 5 4
    runExample "ordered-equations-and-conjunctive-guards" 5 2
    runExample "finite-observation-of-recursive-producer" 6 4
    runExample "ternary-partials-capture-and-composition" 25 6
    runExample "shared-top-level-across-exported-calls" 30 3
    runExample "scanl-unused-input" 5 3
    runExample "lazy-datatype-primitive-field" 5 3
    checkDemands
    runExample "foldr-arity-four-partial" 30 6
    runExample "foldl-named" 10 6
    runExample "zipWith-named" 10 4
    runExample "concatMap-named" 5 6
    runExample "repeatly-external-identity" 10 4
    runExample "mutually-recursive-captured-helpers" 5 6
    runExample "case-pattern-and-let-guards" 10 1
    runExample "literal-pattern-fallback" 5 2
    runExample "char-scalar-field" 5 2
    runExample "literal-pattern-char-string" 14 4
    runExample "direct-composition-application" 20 3
    runExample "polymorphic-shared-top-level-value" 5 3
    runExample "polymorphic-local-value" 10 4
    runExample "resolved-user-map" 10 2
    runExample "unused-error-and-short-circuit" 5 1
    runExample "record-lazy-fields-and-selectors" 10 3
    runExample "nested-pair-and-maybe-fields" 5 3
    runExample "nested-case-retains-outer-binding" 15 1
    runExample "named-source-operator" 10 2
    runExample "infix-constructor" 5 2
    checkDiagnostics
    checkRuntimeFailures
    checkGeneratedProperties
    runExample "direct-foldr-maybe" (-5) 3
    runExample "direct-foldl-pair-unused-first-field" 5 3
    runExample "direct-foldr-nothing-unused-callback" 5 2
    checkPatternScaling
    runExample "scalar-division-and-negation" 6 1
    runExample "direct-concatMap-maybe" 5 7
    runExample "explicit-constructor-exports" 5 3
    checkHigherKindedAudit
    checkHigherKindedDemands
    runExample "datatype-name-shadows-foldable-class" 5 2
    runExample "constrained-sharing-different-dictionaries" 12 5
    runAction "constrained-sharing-same-dictionary" "constrained-sharing-different-dictionaries/Example.hs" 2 3
    runExample "constrained-sharing-unconstrained-parameter" 2 3
    runExample "constrained-sharing-local" 5 6
    runExample "constrained-sharing-generic-caller" 12 7
    runExample "constrained-sharing-ignored-signature" 5 2
    runExample "constrained-sharing-scoped-signature" 5 2
    runExample "constrained-sharing-local-scoped-signature" 5 3
    runExample "constrained-sharing-defaulted-use" 5 2
    runExample "constrained-sharing-constructor-dictionaries" 12 4
    runExample "constrained-sharing-all-dictionaries" 20 4
    runExample "constrained-sharing-nested-allocations" 12 7
    runExample "injective-value-names" 16 3
    runExample "injective-constructor-names" 15 3
    runExample "injective-type-and-helper-names" 10 3
    runExample "identifier-escaping-magic-hash" 5 2
    runExample "record-selector-prelude-collision" 5 2
    runExample "record-pattern-source-order" 2 2
    checkGeneratedWarnings
    checkGeneratedModuleNameDiagnostic
    runExample "composition-partial-map" 30 12
    runExample "composition-prefix" 20 5
    runExample "composition-generalized-partial" 40 7
    runExample "composition-nested-partial-sharing" 50 13
    runExample "composition-captured-operands" 70 16
    runExample "composition-arity-two-right-operand" 10 6

checkThunkForwarding :: IO ()
checkThunkForwarding = do
    selected <- getArgs
    when (null selected || "thunk-forwarding" `elem` selected) $ do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-thunk-forwarding-"
      putStrLn ("Thunk-forwarding fixtures and logs: " ++ root)
      copyFixture "thunk-forwarding/Example.hs" (root </> "Example.hs")
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      copyFixture "thunk-forwarding/Main.hs" (root </> "Main.hs")
      _ <- compileAndRun root
      putStrLn "PASS: thunk-forwarding / unused argument / two ticks / no forwarding allocation"

checkThunkMatching :: IO ()
checkThunkMatching = do
    selected <- getArgs
    mapM_ (check selected)
        ["thunk-case", "thunk-guards"]
  where
    check selected label = when (null selected || label `elem` selected) $ do
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
    selected <- getArgs
    when (null selected || "anf-demand" `elem` selected) $ do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-anf-demand-"
      putStrLn ("ANF-demand fixtures and logs: " ++ root)
      copyFixture "anf-demand/Example.hs" (root </> "Example.hs")
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      copyFixture "anf-demand/Main.hs" (root </> "Main.hs")
      _ <- compileAndRun root
      putStrLn "PASS: anf-demand / fresh suspension, explicit anchors, ordered matching, scalar and Boolean demand"

checkThunkFields :: IO ()
checkThunkFields = do
    selected <- getArgs
    when (null selected || "thunk-fields" `elem` selected) $ do
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
    selected <- getArgs
    when (null selected || "thunk-captures" `elem` selected) $ do
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
    selected <- getArgs
    when (null selected || "thunk-sharing" `elem` selected) $ do
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
    selected <- getArgs
    when (null selected || "generated-module-name-diagnostic" `elem` selected) $ do
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
    selected <- getArgs
    when (null selected || "generated-warnings" `elem` selected) $ do
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
    selected <- getArgs
    when (null selected || "higher-kinded-demands" `elem` selected) $ do
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
checkHigherKindedAudit :: IO ()
checkHigherKindedAudit = do
    failures <- mapM check
        ["higher-kinded-datatype-field"
        , "higher-kinded-recursive-field"
        , "higher-kinded-polykinded-field"
        ]
    unless (all null failures) (fail (unlines (filter (not . null) failures)))
  where
    check label = do
        result <- try (runExample label 5 3) :: IO (Either SomeException ())
        pure (either (\e -> label ++ ": " ++ displayException e) (const "") result)

checkPatternScaling :: IO ()
checkPatternScaling = do
    selected <- getArgs
    when (null selected || "pattern-scaling" `elem` selected) $ do
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
    selected <- getArgs
    when (null selected || "anf-properties" `elem` selected) $ do
      tmp <- getTemporaryDirectory
      root <- createTempDirectory tmp "cost-translate-anf-properties-"
      putStrLn ("ANF property fixtures and logs: " ++ root)
      copyFixture "anf-properties/Example.hs" (root </> "Example.hs")
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      copyFixture "anf-properties/Main.hs" (root </> "Main.hs")
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
      copyFixture "thunk-matching-sharing/Example.hs" (root </> "Example.hs")
      translated <- translateFile (root </> "Example.hs") (root </> "Generated.hs")
      either fail pure translated
      copyFixture "thunk-matching-sharing/Main.hs" (root </> "Main.hs")
      _ <- compileAndRun root
      putStrLn "PASS: thunk-matching-sharing / polymorphic and dictionary identities / scoped and defaulted evidence / suspended bottoms / repeated-demand ticks"

checkGeneratedProperties :: IO ()
checkGeneratedProperties = do
    selected <- getArgs
    if not (null selected || "generated-properties" `elem` selected) then pure () else do
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
    selected <- getArgs
    if not (null selected || "runtime-failures" `elem` selected) then pure () else do
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
    selected <- getArgs
    if not (null selected || "diagnostics" `elem` selected) then pure () else do
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
    selected <- getArgs
    if not (null selected || "datatypes-and-demands" `elem` selected) then pure () else do
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
    selected <- getArgs
    when (null selected || label `elem` selected) run
  where
   run = do
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
