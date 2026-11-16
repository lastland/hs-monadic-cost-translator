module Main (main) where

import Control.Monad (forM_, unless, when)
import Cost.Translate (translateFile)
import Data.List (intercalate, isInfixOf, isPrefixOf, nub)
import FixturePolicy (checkFixturePolicy)
import GeneratedChecks (checkGeneratedArtifacts, checkScopedRenamingSources)
import GeneratedConstruction (checkGeneratedConstructionSpecimens)
import GeneratedFunctions (checkGeneratedFunctionSpecimens)
import Paths_cost_translate (getDataFileName)
import ReferencePolicy (checkReferencePolicyTests, diagnosticFixtures, discoverReferenceCases, validateReferenceCases)
import ReferenceRunner
    ( ReferenceEnvironment, checkGeneratedConstructionLifecycle, checkGeneratedFunctionLifecycle, checkReferenceHarness
    , prepareReferenceEnvironment, runReferenceCase
    )
import System.Directory (copyFile, doesFileExist, getCurrentDirectory, getTemporaryDirectory, listDirectory)
import System.Environment (getArgs, getEnvironment, getExecutablePath, setEnv)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (createTempDirectory, withSystemTempDirectory)
import System.Process (CreateProcess(env), proc, readCreateProcessWithExitCode, readProcessWithExitCode)

-- No arbitrary IO callbacks in the registry. Every semantic registration has
-- exactly one execution path, regardless of how its fixture was discovered.
data Group
    = Semantic String [FilePath]
    | Diagnostics
    | WholeModuleFailure
    | GeneratedModuleNameDiagnostic
    | TestSelection
    | FixturePolicy
    | ReferenceHarness
    | GeneratedFunctions
    | GeneratedConstruction

groupName :: Group -> String
groupName (Semantic label _) = label
groupName Diagnostics = "diagnostics"
groupName WholeModuleFailure = "whole-module-failure"
groupName GeneratedModuleNameDiagnostic = "generated-module-name-diagnostic"
groupName TestSelection = "test-selection"
groupName FixturePolicy = "fixture-policy"
groupName ReferenceHarness = "reference-harness"
groupName GeneratedFunctions = "generated-functions"
groupName GeneratedConstruction = "generated-construction"

main :: IO ()
main = do
    -- Cabal installations retain data files removed by later builds. Prefer
    -- the actual checkout when testing there, so stale installed fixtures do
    -- not masquerade as newly added tests. Installed/sdist runs use package data.
    checkout <- doesFileExist "cost-translate.cabal"
    when checkout (getCurrentDirectory >>= setEnv "cost_translate_datadir")
    selected <- getArgs
    cases <- discoverReferenceCases
    let registry = map (uncurry Semantic) cases ++
            [Diagnostics, WholeModuleFailure, GeneratedModuleNameDiagnostic, TestSelection, FixturePolicy, ReferenceHarness, GeneratedFunctions, GeneratedConstruction]
        names = map groupName registry
        unknown = nub (filter (`notElem` names) selected)
    unless (null unknown) $ do
        hPutStrLn stderr ("Unknown test selector(s): " ++ intercalate ", " (map show unknown))
        hPutStrLn stderr "Usage: cost-translate-test [SELECTOR ...] (no selectors runs all groups)"
        hPutStrLn stderr ("Available test selectors: " ++ intercalate ", " names)
        exitFailure
    -- Validate the entire inventory even when only one group was requested.
    validateReferenceCases
    let requested = filter (\group -> null selected || groupName group `elem` selected) registry
    environment <- if any needsRuntime requested
        then Just <$> prepareReferenceEnvironment
        else pure Nothing
    mapM_ (runGroup environment) requested

needsRuntime :: Group -> Bool
needsRuntime Semantic{} = True
needsRuntime ReferenceHarness = True
needsRuntime GeneratedFunctions = True
needsRuntime GeneratedConstruction = True
needsRuntime _ = False

runGroup :: Maybe ReferenceEnvironment -> Group -> IO ()
runGroup environment (Semantic label sources) = do
    when (label == "anf-scoped-renaming") checkScopedRenamingSources
    context <- requireEnvironment environment
    artifacts <- runReferenceCase context (label, sources)
    mapM_ (checkGeneratedArtifacts label) artifacts
    putStrLn ("PASS: " ++ label ++ " / handwritten reference / boundary cases and 200 randomized comparisons")
runGroup _ Diagnostics = checkDiagnostics
runGroup _ WholeModuleFailure = checkWholeModuleFailure
runGroup _ GeneratedModuleNameDiagnostic = checkGeneratedModuleNameDiagnostic
runGroup _ TestSelection = checkTestSelection
runGroup _ FixturePolicy = checkFixturePolicy >> checkReferencePolicyTests
runGroup environment ReferenceHarness = requireEnvironment environment >>= checkReferenceHarness
runGroup environment GeneratedFunctions = do
    checkGeneratedFunctionSpecimens
    requireEnvironment environment >>= checkGeneratedFunctionLifecycle
runGroup environment GeneratedConstruction = do
    checkGeneratedConstructionSpecimens
    requireEnvironment environment >>= checkGeneratedConstructionLifecycle

requireEnvironment :: Maybe ReferenceEnvironment -> IO ReferenceEnvironment
requireEnvironment = maybe (fail "internal error: reference environment was not prepared") pure

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
        ["generated-module-name-diagnostic", "whole-module-failure"] [wholeModulePass, moduleNamePass]
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
        [ "selectors: " ++ show selectors, "exit status: " ++ show status
        , "stdout: " ++ show out, "stderr: " ++ show err ]

checkGeneratedModuleNameDiagnostic :: IO ()
checkGeneratedModuleNameDiagnostic = do
    tmp <- getTemporaryDirectory
    root <- createTempDirectory tmp "cost-translate-generated-parse-"
    let source = root </> "Example.hs"
        output = root </> "bad-name.hs"
    copyFixture "identity/Example.hs" source
    (status,out,err) <- readProcessWithExitCode "cost-translate" [source,output] ""
    logText <- readFile (output ++ ".log")
    published <- doesFileExist output
    unless (status /= ExitSuccess && not published &&
            "generated module name is invalid" `isInfixOf` (out ++ err) &&
            not ("unsupported source" `isInfixOf` logText) && "Failed candidate:" `isInfixOf` logText) $
        fail ("wrong generated module-name diagnostic: " ++ logText)
    putStrLn "PASS: invalid generated module names retain their category and candidate"

checkDiagnostics :: IO ()
checkDiagnostics = do
    forM_ diagnosticFixtures $ \(label, message) -> do
        tmp <- getTemporaryDirectory
        root <- createTempDirectory tmp "cost-translate-diagnostic-"
        let source = root </> "Example.hs"
            output = root </> "Generated.hs"
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
            unless (any (\line -> "Example.hs:2:" `isInfixOf` line && message `isInfixOf` line) (lines logText)) $
                fail ("local signature diagnostic must include its source location: " ++ logText)
        putStrLn ("PASS: CLI " ++ label)
    tmp <- getTemporaryDirectory
    root <- createTempDirectory tmp "cost-translate-missing-runtime-"
    copyFixture "identity/Example.hs" (root </> "Example.hs")
    (status, out, err) <- readProcessWithExitCode "/usr/bin/env"
        ["-u", "GHC_PACKAGE_PATH", "-u", "GHC_ENVIRONMENT", "cost-translate", root </> "Example.hs", root </> "Generated.hs"] ""
    published <- doesFileExist (root </> "Generated.hs")
    unless (status /= ExitSuccess && not published && "setup error" `isInfixOf` (out ++ err)) $
        fail ("missing runtime diagnostic: " ++ show (status,out,err))
    putStrLn "PASS: CLI missing runtime is a setup error"

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

copyFixture :: FilePath -> FilePath -> IO ()
copyFixture fixture destination = do
    source <- getDataFileName ("test" </> "cases" </> fixture)
    copyFile source destination
