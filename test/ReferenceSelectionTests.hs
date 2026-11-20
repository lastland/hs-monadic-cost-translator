-- | Real public CLI tests; run with reference-harness, not runtime-free Cabal.
module ReferenceSelectionTests (checkReferenceSelection, checkReferenceListing) where

import Control.Monad (unless)
import Control.Exception (evaluate)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (fromMaybe)
import ReferencePolicy (discoverReferenceCases)
import ReferenceSelection (referenceCases, caseKey)
import System.Environment (getEnvironment, getExecutablePath)
import System.Directory
    ( Permissions(..), createDirectoryIfMissing, findExecutable, getPermissions, setPermissions )
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.IO (hGetContents, hGetLine)
import System.Process
    ( CreateProcess(env, std_out, std_err, create_group), StdStream(CreatePipe)
    , proc, readCreateProcessWithExitCode, readProcessWithExitCode
    , withCreateProcess, interruptProcessGroupOf, waitForProcess )
import System.Timeout (timeout)

checkReferenceSelection :: IO ()
checkReferenceSelection = do
    executable <- getExecutablePath
    (referenceStatus, referenceOut, referenceErr) <- readProcessWithExitCode executable
        ["anf-scoped-renaming", "identity", "anf-scoped-renaming"] ""
    let shadowedKey = "anf-scoped-renaming:test/cases/anf-scoped-renaming/Shadowed.hs"
        renamedKey = "anf-scoped-renaming:test/cases/anf-scoped-renaming/Renamed.hs"
        identityKey = "identity:test/cases/identity/Example.hs"
        referenceKeys =
            [renamedKey
            ,shadowedKey
            ,identityKey]
        reports = filter (\line -> any (`isInfixOf` line) referenceKeys &&
            any (`isPrefixOf` line) ["PASS: ", "FAIL: "]) (lines referenceOut)
    unless (referenceStatus /= ExitSuccess &&
            reports == ["FAIL: " ++ renamedKey, "FAIL: " ++ shadowedKey, "PASS: " ++ identityKey] &&
            "Reference summary: 1 passed, 2 failed" `isInfixOf` referenceOut &&
            "Artifacts:" `isInfixOf` (referenceOut ++ referenceErr)) $
        fail ("reference selection must isolate every source, continue after failure and retain strict status\n" ++
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
    (knownStatus, knownOut, knownErr) <- readProcessWithExitCode executable
        ["--known-failures", renamedKey] ""
    unless (knownStatus /= ExitSuccess &&
            filter ("FAIL: " `isPrefixOf`) (lines knownOut) == ["FAIL: " ++ renamedKey] &&
            "Reference summary: 0 passed, 1 failed" `isInfixOf` knownOut &&
            "known failures remaining: 1" `isInfixOf` knownOut &&
            "Known failure remains:" `isInfixOf` knownErr) $
        fail ("known failures must execute unchanged assertions and retain nonzero status\n" ++
            childResult knownStatus knownOut knownErr)
    (sourceStatus, sourceOut, sourceErr) <- readProcessWithExitCode executable
        ["--all-references", renamedKey, identityKey, renamedKey] ""
    unless (sourceStatus /= ExitSuccess &&
            filter (\line -> any (`isPrefixOf` line) ["PASS: ", "FAIL: "]) (lines sourceOut) ==
                ["FAIL: " ++ renamedKey, "PASS: " ++ identityKey] &&
            "Reference summary: 1 passed, 1 failed" `isInfixOf` sourceOut) $
        fail ("source-key selectors must isolate one variant and retain registry order and strict status\n" ++
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
    checkCrashedCompiler executable
    putStrLn "PASS: reference-selection / per-source isolation, strict failures, continuation, listing and mode selection"

checkCrashedCompiler :: FilePath -> IO ()
checkCrashedCompiler executable =
    withSystemTempDirectory "reference-selection-crash-" $ \root -> do
        realGhc <- findExecutable "ghc" >>= maybe (fail "compiler crash test needs ghc") pure
        environment <- getEnvironment
        let commands = root </> "commands"
            wrapper = commands </> "ghc"
            injected =
                [("PATH", commands ++ ":" ++ fromMaybe "" (lookup "PATH" environment))
                ,("REFERENCE_REAL_GHC", realGhc)]
            child = (proc executable ["--all-references", "anf-scoped-renaming", "identity"])
                {env = Just (injected ++ filter (\(key, _) -> key `notElem` map fst injected) environment)}
        createDirectoryIfMissing True commands
        writeFile wrapper (unlines
            ["#!/bin/sh", "case \"$*\" in *Generated.hs*) kill -TERM \"$$\" ;; esac"
            ,"exec \"$REFERENCE_REAL_GHC\" \"$@\""])
        permissions <- getPermissions wrapper
        setPermissions wrapper permissions {executable = True}
        (status, out, err) <- readCreateProcessWithExitCode child ""
        unless (status /= ExitSuccess &&
                "setup error: generated module compiler crashed" `isInfixOf` err &&
                not (any (`isInfixOf` out) ["FAIL: ", "PASS: ", "Reference summary:"]) &&
                length (filter ("Reference fixtures and logs:" `isPrefixOf`) (lines out)) == 1) $
            fail ("compiler crashes must stop the suite, not become deferred failures\n" ++ childResult status out err)

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

checkAccepted :: FilePath -> [String] -> [String] -> IO ()
checkAccepted executable selectors expectedPasses = do
    (status, out, err) <- readProcessWithExitCode executable selectors ""
    unless (status == ExitSuccess && filter ("PASS: " `isPrefixOf`) (lines out) == expectedPasses) $
        fail ("reference selection ran unexpected cases: " ++ show selectors ++ "\n" ++
            childResult status out err)

childResult :: ExitCode -> String -> String -> String
childResult status out err = unlines
    ["exit status: " ++ show status, "stdout: " ++ show out, "stderr: " ++ show err]
