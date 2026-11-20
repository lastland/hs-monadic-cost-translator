-- | Per-source reporting around the existing enforced reference lifecycle.
module ReferenceExecution (runReferences) where

import Control.Exception (IOException, catch, throwIO)
import Control.Monad (forM, when)
import Data.List (isInfixOf)
import GeneratedChecks (checkGeneratedArtifacts, checkScopedRenamingSources)
import ReferenceRunner
    ( ReferenceEnvironment, Artifacts(..), runReferenceCase, checkOptimizedReferenceCases
    , retainReferenceFailure )
import ReferenceSelection (ReferenceCase(..), KnownFailure(..), caseKey, knownFailure)
import ScalarComparisonChecks (checkScalarComparisonCallers)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isUserError)
import System.FilePath ((</>), takeDirectory)
import System.Directory (getTemporaryDirectory)
import System.IO.Temp (createTempDirectory)

runReferences :: ReferenceEnvironment -> [ReferenceCase] -> IO Bool
runReferences environment cases = do
    results <- forM cases $ \item -> do
        result <- (run item >> pure True) `catch` reportFailure item
        when result $ do
            putStrLn ("PASS: " ++ caseKey item)
            case knownFailure item of
                Nothing -> pure ()
                Just _ -> putStrLn ("Recovered known failure: " ++ caseKey item ++
                    " (manual promotion required; catalog unchanged)")
        pure result
    let passed = length (filter id results)
        failed = length results - passed
    putStrLn ("Reference summary: " ++ show passed ++ " passed, " ++ show failed ++ " failed")
    pure (and results)
  where
    run item = do
        let label = caseLabel item
        when (label == "anf-scoped-renaming") checkScopedRenamingSources
        artifacts <- runReferenceCase environment (label, [caseSource item])
        mapM_ (\artifact -> retainReferenceFailure (takeDirectory (generatedDirectory artifact)) $ do
            when (label == "caller-scalar-comparisons")
                (checkScalarComparisonCallers environment [artifact])
            when (label == "recursive-tree-representation")
                (checkOptimizedReferenceCases "recursive Tree reprM" environment [artifact])
            when (label == "runtime-demand-bundle")
                (checkOptimizedReferenceCases "acyclic runtime demand bundle" environment [artifact])
            when (label == "higher-kinded-stock-context")
                (checkOptimizedReferenceCases "higher-kinded stock context" environment [artifact])
            checkGeneratedArtifacts label artifact) artifacts
    reportFailure :: ReferenceCase -> IOException -> IO Bool
    reportFailure item exception
        -- Filesystem/process failures and translator setup failures are fatal.
        -- Catching IOException alone deliberately leaves asynchronous exceptions,
        -- including UserInterrupt, untouched.
        | not (isUserError exception) || any (`isInfixOf` show exception)
            ["setup error:", "construction setup:", "cannot satisfy -package",
             "can't find a package database", "stg_ap_pp_ret", "ghc: panic!"] =
            throwIO exception
        | otherwise = do
            temporary <- getTemporaryDirectory
            root <- createTempDirectory temporary "cost-reference-failure-"
            let logPath = root </> "failure.log"
            writeFile logPath (caseKey item ++ "\n" ++ show exception)
            putStrLn ("FAIL: " ++ caseKey item)
            case knownFailure item of
                Nothing -> hPutStrLn stderr "Success case regression (not added to known failures)."
                Just record -> hPutStrLn stderr ("Known failure remains: " ++
                    failureReason record ++ " (" ++ failureIssue record ++ ")")
            hPutStrLn stderr (caseKey item ++ ": " ++ show exception ++
                "\nFailure log: " ++ logPath)
            pure False
