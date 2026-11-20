module Main (main) where

import Control.Monad (forM_, unless, when)
import CapturedKeySharing (checkCapturedKeySharing)
import ClosedLocalComparisonChecks (checkClosedLocalComparisons)
import Cost.Translate (translateFile)
import Data.List (intercalate, isInfixOf, isPrefixOf, nub)
import FixturePolicy (checkFixturePolicy)
import GeneratedConstruction (checkGeneratedConstructionSpecimens)
import GeneratedFunctions (checkGeneratedFunctionSpecimens)
import Paths_cost_translate (getDataFileName)
import ProducerSharing (checkProducerSharing)
import ReferencePolicy (checkReferencePolicyTests, diagnosticFixtures, discoverReferenceCases)
import ReferenceExecution (runReferences)
import ReferenceSelectionTests (checkReferenceSelection, checkReferenceListing)
import ReferenceSelection
    ( ReferenceMode(..), referenceCases, caseKey, selectReferences, listReference
    )
import ReferenceRunner
    ( ReferenceEnvironment, checkGeneratedConstructionLifecycle, checkGeneratedFunctionLifecycle, checkReferenceHarness
    , prepareReferenceEnvironment, checkSourceScopeLifecycle
    )
import RuntimeAllocator (checkRuntimeAllocator)
import System.Directory (copyFile, doesFileExist, getCurrentDirectory, getTemporaryDirectory, listDirectory)
import System.Environment (getArgs, getEnvironment, getExecutablePath, setEnv)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>))
import System.IO (BufferMode(LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
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
    | RuntimeAllocator
    | ProducerSharing
    | CapturedKeySharing
    | SourceScope

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
groupName RuntimeAllocator = "runtime-allocator"
groupName ProducerSharing = "opaque-producer-sharing"
groupName CapturedKeySharing = "captured-key-sharing"
groupName SourceScope = "source-scope"

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    -- Cabal installations retain data files removed by later builds. Prefer
    -- the actual checkout when testing there, so stale installed fixtures do
    -- not masquerade as newly added tests. Installed/sdist runs use package data.
    checkout <- doesFileExist "cost-translate.cabal"
    when checkout (getCurrentDirectory >>= setEnv "cost_translate_datadir")
    arguments <- getArgs
    cases <- discoverReferenceCases
    let registry = map (uncurry Semantic) cases ++
            [Diagnostics, WholeModuleFailure, GeneratedModuleNameDiagnostic, TestSelection, FixturePolicy, ReferenceHarness, GeneratedFunctions, GeneratedConstruction, RuntimeAllocator, ProducerSharing, CapturedKeySharing, SourceScope]
        inventory = referenceCases cases
        modes = [("--success", Success), ("--known-failures", KnownFailures), ("--all-references", AllReferences)]
        options = "--list-reference-cases" : map fst modes
        selected = filter (`notElem` options) arguments
        selectedModes = [chosen | (option, chosen) <- modes, option `elem` arguments]
        listing = "--list-reference-cases" `elem` arguments
        mode = case selectedModes of
            [chosen] -> chosen
            _ | null arguments -> Success
              | otherwise -> AllReferences
        names = map groupName registry ++ map caseKey inventory
        unknown = nub (filter (`notElem` names) selected)
    unless (null unknown && length selectedModes <= 1) $ do
        unless (null unknown) $
            hPutStrLn stderr ("Unknown test selector(s): " ++ intercalate ", " (map show unknown))
        when (length selectedModes > 1) $
            hPutStrLn stderr "Conflicting reference modes: choose only one."
        hPutStrLn stderr "Usage: cost-translate-test [--success | --known-failures | --all-references] [--list-reference-cases] [SELECTOR ...]"
        hPutStrLn stderr ("Available test selectors: " ++ intercalate ", " names)
        exitFailure
    -- Discovery validates the entire inventory and catalog even for one selector.
    let infrastructure = [group | group <- registry, not (isSemantic group)]
        collisions = [label | (label, _) <- cases,
            label `elem` map groupName infrastructure || "--" `isPrefixOf` label]
        references = selectReferences mode selected inventory
        requested = filter (\group -> null arguments || groupName group `elem` selected) infrastructure
    unless (null collisions) (fail ("reference labels collide with reserved selectors: " ++ show collisions))
    if listing then mapM_ (putStrLn . listReference) references
    else do
        environment <- if not (null references) || any needsReferenceEnvironment requested
            then Just <$> prepareReferenceEnvironment
            else pure Nothing
        succeeded <- if null references then do
                unless (null selectedModes) (putStrLn "Reference summary: 0 passed, 0 failed")
                pure True
            else requireEnvironment environment >>= \context -> runReferences context references
        mapM_ (runGroup environment) requested
        unless succeeded exitFailure

isSemantic :: Group -> Bool
isSemantic Semantic{} = True
isSemantic _ = False

needsReferenceEnvironment :: Group -> Bool
needsReferenceEnvironment Semantic{} = True
needsReferenceEnvironment ReferenceHarness = True
needsReferenceEnvironment GeneratedFunctions = True
needsReferenceEnvironment GeneratedConstruction = True
needsReferenceEnvironment ProducerSharing = True
needsReferenceEnvironment CapturedKeySharing = True
needsReferenceEnvironment SourceScope = True
needsReferenceEnvironment _ = False

runGroup :: Maybe ReferenceEnvironment -> Group -> IO ()
runGroup _ Semantic{} = fail "internal error: semantic group bypassed per-source runner"
runGroup _ Diagnostics = checkDiagnostics
runGroup _ WholeModuleFailure = checkWholeModuleFailure
runGroup _ GeneratedModuleNameDiagnostic = checkGeneratedModuleNameDiagnostic
runGroup _ TestSelection = checkTestSelection
runGroup _ FixturePolicy = checkFixturePolicy >> checkReferencePolicyTests
runGroup environment ReferenceHarness = do
    requireEnvironment environment >>= checkReferenceHarness
    checkReferenceSelection
runGroup environment GeneratedFunctions = do
    checkGeneratedFunctionSpecimens
    requireEnvironment environment >>= checkGeneratedFunctionLifecycle
runGroup environment GeneratedConstruction = do
    checkClosedLocalComparisons
    checkGeneratedConstructionSpecimens
    requireEnvironment environment >>= checkGeneratedConstructionLifecycle
runGroup _ RuntimeAllocator = checkRuntimeAllocator
runGroup environment ProducerSharing = requireEnvironment environment >>= checkProducerSharing
runGroup environment CapturedKeySharing = requireEnvironment environment >>= checkCapturedKeySharing
runGroup environment SourceScope = requireEnvironment environment >>= checkSourceScopeLifecycle

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
    checkRejected executable ["--all-references", unknown] [unknown]
    checkRejected executable ["--list-reference-cases", unknown] [unknown]
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
    checkReferenceListing
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
    let unavailable = "setup error: target modules are unavailable in GHC's environment"
    unless (status == ExitFailure 1 && not published &&
            unavailable `isInfixOf` (out ++ err)) $
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
