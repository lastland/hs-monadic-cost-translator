-- | Real public CLI tests; run with reference-harness, not runtime-free Cabal.
module ReferenceSelectionTests (checkReferenceSelection, checkReferenceListing) where

import Control.Monad (unless)
import Control.Exception (evaluate)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (fromMaybe)
import ReferencePolicy (discoverReferenceCases)
import ReferenceSelection (referenceCases, caseKey, knownFailures, failureKey)
import Paths_cost_translate (getDataFileName)
import System.Environment (getEnvironment, getExecutablePath)
import System.Directory
    ( Permissions(..), createDirectoryIfMissing, findExecutable, getPermissions, setPermissions
    , createDirectoryLink, copyFile )
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.IO (hGetContents, hGetLine)
import System.Process
    ( CreateProcess(env, cwd, std_out, std_err, create_group), StdStream(CreatePipe)
    , proc, readCreateProcessWithExitCode, readProcessWithExitCode
    , withCreateProcess, interruptProcessGroupOf, waitForProcess )
import System.Timeout (timeout)

checkReferenceSelection :: IO ()
checkReferenceSelection = do
    environment <- getEnvironment
    unless (lookup "REFERENCE_NEGATIVE_CRASH_CHILD" environment == Just "1") checkSelection

checkSelection :: IO ()
checkSelection = do
    executable <- getExecutablePath
    checkInjectedContinuation executable
    checkNegativeComparerCrash executable
    let recovered arguments = withComparisonOutcome True executable "" ($ arguments)
    (referenceStatus, referenceOut, referenceErr) <- recovered
        ["anf-scoped-renaming", "identity", "anf-scoped-renaming"]
    let shadowedKey = "anf-scoped-renaming:test/cases/anf-scoped-renaming/Shadowed.hs"
        renamedKey = "anf-scoped-renaming:test/cases/anf-scoped-renaming/Renamed.hs"
        identityKey = "identity:test/cases/identity/Example.hs"
        deferredKeys = map failureKey knownFailures
        recoveredCount = length (filter (`elem` deferredKeys) [renamedKey, shadowedKey])
        referenceKeys =
            [renamedKey
            ,shadowedKey
            ,identityKey]
        reports = filter (\line -> any (`isInfixOf` line) referenceKeys &&
            any (`isPrefixOf` line) ["PASS: ", "FAIL: "]) (lines referenceOut)
    unless (referenceStatus == ExitSuccess &&
            reports == ["PASS: " ++ renamedKey, "PASS: " ++ shadowedKey, "PASS: " ++ identityKey] &&
            "Reference summary: 3 passed, 0 failed" `isInfixOf` referenceOut &&
            ("recovered: " ++ show recoveredCount ++ " (manual promotion required)") `isInfixOf` referenceOut) $
        fail ("reference selection must isolate every source and accept recoveries without promotion\n" ++
            childResult referenceStatus referenceOut referenceErr)
    (listedStatus, listedOut, listedErr) <- readProcessWithExitCode executable
        ["--list-reference-cases", "identity", "anf-scoped-renaming"] ""
    unless (listedStatus == ExitSuccess &&
            all (`isInfixOf` listedOut) referenceKeys &&
            "success\tidentity:test/cases/identity/Example.hs" `isInfixOf` listedOut &&
            "sh scripts/stack-test.sh cost-translate --test-arguments='identity:test/cases/identity/Example.hs'" `isInfixOf` listedOut &&
            not ("Reference fixtures and logs:" `isInfixOf` listedOut)) $
        fail ("reference listing must expose every source and a strict rerun command without execution\n" ++
            childResult listedStatus listedOut listedErr)
    checkAccepted executable ["--success", "identity", "identity"]
        ["PASS: identity:test/cases/identity/Example.hs"]
    checkAccepted executable ["--known-failures", "identity"] []
    (knownStatus, knownOut, knownErr) <- recovered
        ["--known-failures", renamedKey]
    unless (knownStatus == ExitSuccess &&
            (if renamedKey `elem` deferredKeys then
                filter ("PASS: " `isPrefixOf`) (lines knownOut) == ["PASS: " ++ renamedKey] &&
                "Reference summary: 1 passed, 0 failed" `isInfixOf` knownOut &&
                "known failures remaining: 0, recovered: 1" `isInfixOf` knownOut &&
                "Recovered known failure:" `isInfixOf` knownOut
             else not ("Reference fixtures and logs:" `isInfixOf` knownOut))) $
        fail ("known failures must accept recoveries without automatically changing classification\n" ++
            childResult knownStatus knownOut knownErr)
    (sourceStatus, sourceOut, sourceErr) <- recovered
        ["--all-references", renamedKey, identityKey, renamedKey]
    unless (sourceStatus == ExitSuccess &&
            filter (\line -> any (`isPrefixOf` line) ["PASS: ", "FAIL: "]) (lines sourceOut) ==
                ["PASS: " ++ renamedKey, "PASS: " ++ identityKey] &&
            "Reference summary: 2 passed, 0 failed" `isInfixOf` sourceOut) $
        fail ("source-key selectors must isolate one variant and retain registry order\n" ++
            childResult sourceStatus sourceOut sourceErr)
    withSystemTempDirectory "reference-selection-setup-" $ \emptyPath -> do
        environment <- getEnvironment
        let child = (proc executable ["--all-references", "anf-scoped-renaming", "identity"])
                {env = Just (("PATH", emptyPath) : filter ((/= "PATH") . fst) environment)}
        (status, out, err) <- readCreateProcessWithExitCode child ""
        unless (status /= ExitSuccess && "setup error:" `isInfixOf` err &&
                not (any (`isInfixOf` out) ["FAIL: ", "PASS: ", "Reference summary:"])) $
            fail ("setup errors must be fatal, not reported as reference regressions\n" ++ childResult status out err)
    checkInterrupted executable
    checkCandidateCompilerCrashes executable
    checkMissingRuntimeCompilerCrash executable
    putStrLn "PASS: reference-selection / per-source isolation, strict failures, continuation, listing and mode selection"

checkInjectedContinuation :: FilePath -> IO ()
checkInjectedContinuation executable = do
    withComparisonFailure executable "test/cases/top-level-function-interface/Control.hs" $ \run -> do
        (status, out, err) <- run
            ["top-level-function-interface", "identity", "top-level-function-interface"]
        unless (status /= ExitSuccess &&
                filter (\line -> any (`isPrefixOf` line) ["PASS: ", "FAIL: "]) (lines out) ==
                    ["PASS: identity:test/cases/identity/Example.hs"
                    ,"FAIL: top-level-function-interface:test/cases/top-level-function-interface/Control.hs"
                    ,"PASS: top-level-function-interface:test/cases/top-level-function-interface/WithValue.hs"] &&
                "Reference summary: 2 passed, 1 failed" `isInfixOf` out &&
                "Artifacts:" `isInfixOf` err &&
                "injected comparison failure: setup error: ordinary payload" `isInfixOf` err) $
            fail ("ordinary comparison payloads must not change fatal setup classification\n" ++
                childResult status out err)
    case knownFailures of
        [] -> pure ()
        record:_ -> withComparisonFailure executable "" $ \run -> do
            let key = failureKey record
            (status, out, err) <- run ["--known-failures", key]
            unless (status /= ExitSuccess &&
                    filter ("FAIL: " `isPrefixOf`) (lines out) == ["FAIL: " ++ key] &&
                    "Reference summary: 0 passed, 1 failed" `isInfixOf` out &&
                    "known failures remaining: 1" `isInfixOf` out &&
                    "Known failure remains:" `isInfixOf` err &&
                    "generated-code type error" `isInfixOf` err) $
                fail ("injected known failures must retain strict nonzero status\n" ++ childResult status out err)

withComparisonFailure :: FilePath -> FilePath ->
    (([String] -> IO (ExitCode, String, String)) -> IO a) -> IO a
withComparisonFailure = withComparisonOutcome False

withComparisonOutcome :: Bool -> FilePath -> FilePath ->
    (([String] -> IO (ExitCode, String, String)) -> IO a) -> IO a
withComparisonOutcome recovered executable source action =
    withSystemTempDirectory "reference-selection-comparison-" $ \root -> do
        realGhc <- findExecutable "ghc" >>= maybe (fail "comparison injection needs ghc") pure
        fixture <- getDataFileName source
        environment <- getEnvironment
        let commands = root </> "commands"
            wrapper = commands </> "ghc"
            injected =
                [("PATH", commands ++ ":" ++ fromMaybe "" (lookup "PATH" environment))
                ,("REFERENCE_REAL_GHC", realGhc), ("REFERENCE_FAILURE_SOURCE", if null source then "" else fixture)
                ,("REFERENCE_RECOVERY", if recovered then "1" else "0")]
            run arguments = readCreateProcessWithExitCode
                (proc executable arguments)
                    {env = Just (injected ++ filter (\(key, _) -> key `notElem` map fst injected) environment)} ""
        createDirectoryIfMissing True commands
        writeFile wrapper (compilerWrapper "\"$REFERENCE_REAL_GHC\" \"$@\""
            ["if [ \"$REFERENCE_RECOVERY\" = 0 ] && [ -z \"$REFERENCE_FAILURE_SOURCE\" ]; then"
            ,"  case \"$*\" in *-fno-code*Generated.hs*)"
            ,"    echo 'Example.hs:1:1: error: user payload \"ghc: panic!\" and \"ghc-9.10.3: panic!\"' >&2; exit 1 ;; esac"
            ,"fi"]
            ["if [ \"$REFERENCE_RECOVERY\" = 1 ]; then exec \"$0.real\" \"$2\" \"$2\"; fi"
            ,"sandbox=${1%/*}"
            ,"if /usr/bin/cmp -s \"$REFERENCE_FAILURE_SOURCE\" \"$sandbox/Example.hs\"; then"
            ,"  echo 'injected comparison failure: setup error: ordinary payload' >&2"
            ,"  exit 1", "fi", "exec \"$0.real\" \"$@\""])
        permissions <- getPermissions wrapper
        setPermissions wrapper permissions {executable = True}
        action run

compilerWrapper :: String -> [String] -> [String] -> String
compilerWrapper compileCommand beforeCompilation comparerBody = unlines $
    ["#!/bin/sh", "set -eu"] ++ beforeCompilation ++
    [compileCommand
    ,"output=", "previous=", "for argument do"
    ,"  if [ \"$previous\" = -o ]; then output=$argument; fi"
    ,"  previous=$argument", "done"
    ,"case \"$output\" in */compare)"
    ,"  /bin/mv \"$output\" \"$output.real\""
    ,"  /bin/cat > \"$output\" <<'COMPARER'"
    ,"#!/bin/sh"] ++ comparerBody ++
    ["COMPARER", "  /bin/chmod +x \"$output\"", ";;", "esac"]

checkNegativeComparerCrash :: FilePath -> IO ()
checkNegativeComparerCrash executable =
    withSystemTempDirectory "reference-negative-crash-" $ \root -> do
        realGhc <- findExecutable "ghc" >>= maybe (fail "negative crash test needs ghc") pure
        environment <- getEnvironment
        let wrapper = root </> "ghc"
            injected =
                [("PATH", root ++ ":" ++ fromMaybe "" (lookup "PATH" environment))
                ,("REFERENCE_REAL_GHC", realGhc)]
        writeFile wrapper (compilerWrapper
            ("'" ++ concatMap (\c -> if c == '\'' then "'\\''" else [c]) realGhc ++ "' \"$@\"") []
            ["case \"$1\" in *WrongValue.hs-*) kill -TERM \"$$\" ;; esac"
            ,"exec \"$0.real\" \"$@\""])
        permissions <- getPermissions wrapper
        setPermissions wrapper permissions {executable = True}
        (status, out, err) <- readCreateProcessWithExitCode
            (proc executable ["reference-harness"])
                {env = Just (("REFERENCE_NEGATIVE_CRASH_CHILD", "1") :
                    injected ++ filter (\(key, _) -> key `notElem` map fst injected) environment)} ""
        unless (status /= ExitSuccess && "process crashed" `isInfixOf` err &&
                not ("PASS: reference-harness" `isInfixOf` out)) $
            fail ("negative specimens must not accept a crashed comparer as a rejection\n" ++
                childResult status out err)
        putStrLn "PASS: reference-harness / negative-specimen comparer crashes are fatal"

checkCandidateCompilerCrashes :: FilePath -> IO ()
checkCandidateCompilerCrashes executable =
    mapM_ check
        [("signal", "kill -TERM \"$$\"")
        ,("exit-1 panic", "echo \"ghc: panic! (the 'impossible' happened)\" >&2; exit 1")
        ,("versioned exit-1 panic", "echo \"ghc-9.10.3: panic! (the 'impossible' happened)\" >&2; exit 1")]
  where
    check (label, crash) =
        withSystemTempDirectory "reference-selection-crash-" $ \root -> do
            realGhc <- findExecutable "ghc" >>= maybe (fail "compiler crash test needs ghc") pure
            environment <- getEnvironment
            let wrapper = root </> "ghc"
                injected =
                    [("PATH", root ++ ":" ++ fromMaybe "" (lookup "PATH" environment))
                    ,("REFERENCE_REAL_GHC", realGhc)]
            writeFile wrapper (compilerWrapper "\"$REFERENCE_REAL_GHC\" \"$@\""
                ["case \"$*\" in *Generated.hs*)", "  " ++ crash ++ " ;; esac"]
                ["exec \"$0.real\" \"$@\""])
            permissions <- getPermissions wrapper
            setPermissions wrapper permissions {executable = True}
            (status, out, err) <- readCreateProcessWithExitCode
                (proc executable ["--all-references", "anf-scoped-renaming", "identity"])
                    {env = Just (injected ++ filter (\(key, _) -> key `notElem` map fst injected) environment)} ""
            unless (status /= ExitSuccess &&
                    "setup error: generated module compiler crashed" `isInfixOf` err &&
                    not (any (`isInfixOf` out) ["FAIL: ", "PASS: ", "Reference summary:"]) &&
                    length (filter ("Reference fixtures and logs:" `isPrefixOf`) (lines out)) == 1) $
                fail ("candidate compiler " ++ label ++ " must stop the suite before later cases\n" ++
                    childResult status out err)
            putStrLn ("PASS: reference-selection / candidate compiler " ++ label ++ " is fatal")

checkMissingRuntimeCompilerCrash :: FilePath -> IO ()
checkMissingRuntimeCompilerCrash executable =
    mapM_ check
        [("signal", "kill -TERM \"$$\"")
        ,("panic", "echo \"ghc: panic! (the 'impossible' happened)\" >&2; exit 1")
        ,("versioned panic", "echo \"ghc-9.10.3: panic! (the 'impossible' happened)\" >&2; exit 1")]
  where
    check (label, crash) =
        withSystemTempDirectory "reference-missing-runtime-crash-" $ \root -> do
            realGhc <- findExecutable "ghc" >>= maybe (fail "diagnostic crash test needs ghc") pure
            environment <- getEnvironment
            let wrapper = root </> "ghc"
                injected =
                    [("PATH", root ++ ":" ++ fromMaybe "" (lookup "PATH" environment))
                    ,("REFERENCE_REAL_GHC", realGhc)]
            writeFile wrapper (compilerWrapper "\"$REFERENCE_REAL_GHC\" \"$@\""
                ["case \"$*\" in *cost-translate-missing-runtime-*Generated.hs*)"
                ,"  " ++ crash ++ " ;; esac"]
                ["exec \"$0.real\" \"$@\""])
            permissions <- getPermissions wrapper
            setPermissions wrapper permissions {executable = True}
            (status, out, err) <- readCreateProcessWithExitCode
                (proc executable ["diagnostics"])
                    {env = Just (injected ++ filter (\(key, _) -> key `notElem` map fst injected) environment)} ""
            unless (status /= ExitSuccess &&
                    "PASS: CLI syntax" `isInfixOf` out &&
                    not ("PASS: CLI missing runtime is a setup error" `isInfixOf` out) &&
                    "missing runtime diagnostic:" `isInfixOf` err &&
                    "generated module compiler crashed" `isInfixOf` err) $
                fail ("missing-runtime diagnostics must reject a compiler " ++ label ++ "\n" ++
                    childResult status out err)
            putStrLn ("PASS: reference-selection / missing-runtime compiler " ++ label ++ " is fatal")

checkInterrupted :: FilePath -> IO ()
checkInterrupted executable =
    withCreateProcess (proc executable ["--all-references", "identity"])
        {std_out = CreatePipe, std_err = CreatePipe, create_group = True} $ \_ output errors child ->
        case (output, errors) of
            (Just outHandle, Just errHandle) -> do
                result <- timeout 60000000 $ do
                    let awaitArtifacts = do
                            line <- hGetLine outHandle
                            unless ("Reference fixtures and logs:" `isPrefixOf` line) awaitArtifacts
                    awaitArtifacts
                    interruptProcessGroupOf child
                    status <- waitForProcess child
                    out <- hGetContents outHandle
                    err <- hGetContents errHandle
                    _ <- evaluate (length out + length err)
                    pure (status, out, err)
                case result of
                    Just (status, out, err) | status /= ExitSuccess &&
                        not (any (`isInfixOf` out) ["FAIL: ", "Reference summary:"]) &&
                        not ("FAIL: " `isInfixOf` err) -> pure ()
                    _ -> fail ("UserInterrupt must propagate without becoming a reference failure: " ++ show result)
            _ -> fail "reference interrupt test requires child output pipes"

checkReferenceListing :: IO ()
checkReferenceListing = do
    executable <- getExecutablePath
    checkMissingIssueTarget executable
    inventory <- map caseKey . referenceCases <$> discoverReferenceCases
    allRows <- listing executable ["--all-references", "--list-reference-cases"]
    successRows <- listing executable ["--success", "--list-reference-cases"]
    knownRows <- listing executable ["--known-failures", "--list-reference-cases"]
    let key row = takeWhile (/= '\t') (drop 1 (dropWhile (/= '\t') row))
        allKeys = map key allRows
        successKeys = map key successRows
        knownKeys = map key knownRows
    unless (allKeys == inventory &&
            filter (`elem` successKeys) allKeys == successKeys &&
            filter (`elem` knownKeys) allKeys == knownKeys &&
            all (\item -> (item `elem` successKeys) /= (item `elem` knownKeys)) inventory &&
            all ("success\t" `isPrefixOf`) successRows &&
            all ("known-failure\t" `isPrefixOf`) knownRows &&
            all ("sh scripts/stack-test.sh cost-translate --test-arguments=" `isInfixOf`) allRows) $
        fail "reference listings must partition the complete discovered source inventory in registry order"
  where
    listing executable arguments = do
        (status, out, err) <- readProcessWithExitCode executable arguments ""
        unless (status == ExitSuccess && not ("Reference fixtures and logs:" `isInfixOf` out)) $
            fail ("listing must not execute references\n" ++ childResult status out err)
        pure (lines out)

checkMissingIssueTarget :: FilePath -> IO ()
checkMissingIssueTarget executable =
    withSystemTempDirectory "reference-issue-target-" $ \root -> do
        environment <- getEnvironment
        let missing = ".scratch/reference-failures/issues/01-source-global-sharing.md"
            issues = root </> ".scratch/reference-failures/issues"
        createDirectoryIfMissing True issues
        mapM_ (\directory -> getDataFileName directory >>= \source ->
            createDirectoryLink source (root </> directory)) ["test", "examples", "integration"]
        getDataFileName "cost-translate.cabal" >>= \source ->
            copyFile source (root </> "cost-translate.cabal")
        -- This is an installation missing its issue data, not a missing checkout.
        -- Discovery must fail before it prints any selectable reference cases.
        (status, out, err) <- readCreateProcessWithExitCode
            (proc executable ["--list-reference-cases"])
                {cwd = Just root, env = Just (("cost_translate_datadir", root) :
                    filter ((/= "cost_translate_datadir") . fst) environment)} ""
        unless (status /= ExitSuccess && null out &&
                ("missing known-failure issue target: " ++ missing) `isInfixOf` err) $
            fail ("reference policy must reject missing packaged issue targets outside the checkout\n" ++
                childResult status out err)

checkAccepted :: FilePath -> [String] -> [String] -> IO ()
checkAccepted executable selectors expectedPasses = do
    (status, out, err) <- readProcessWithExitCode executable selectors ""
    unless (status == ExitSuccess && filter ("PASS: " `isPrefixOf`) (lines out) == expectedPasses) $
        fail ("reference selection ran unexpected cases: " ++ show selectors ++ "\n" ++
            childResult status out err)

childResult :: ExitCode -> String -> String -> String
childResult status out err = unlines
    ["exit status: " ++ show status, "stdout: " ++ show out, "stderr: " ++ show err]
