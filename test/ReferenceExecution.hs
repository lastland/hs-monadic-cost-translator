-- | Per-source reporting around the existing enforced reference lifecycle.
module ReferenceExecution (runReferences) where

import Control.Exception (IOException, catch, throwIO)
import Control.Monad (forM, when)
import Data.Maybe (isJust)
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
        putStrLn ("Reference case: " ++ caseKey item)
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
        knownFailed = length [() | (item, False) <- zip cases results, isJust (knownFailure item)]
        recovered = length [() | (item, True) <- zip cases results, isJust (knownFailure item)]
    putStrLn ("Reference summary: " ++ show passed ++ " passed, " ++ show failed ++ " failed")
    putStrLn ("Reference policy summary: success cases passed: " ++ show (passed - recovered) ++
        ", success case regressions: " ++ show (failed - knownFailed) ++
        "; known failures remaining: " ++ show knownFailed ++
        ", recovered: " ++ show recovered ++ " (manual promotion required)")
    pure (and results)
  where
    run item = do
        let label = caseLabel item
        when (label == "anf-scoped-renaming") checkScopedRenamingSources
        artifacts <- runReferenceCase environment (label, [caseSource item])
        mapM_ (\artifact -> retainReferenceFailure (takeDirectory (generatedDirectory artifact)) $ do
            when (label == "caller-scalar-comparisons")
                (checkScalarComparisonCallers environment [artifact])
            when (label == "mutually-recursive-captured-helpers")
                (checkOptimizedReferenceCases "closed local comparisons" environment [artifact])
            when (label == "recursive-tree-representation")
                (checkOptimizedReferenceCases "recursive Tree reprM" environment [artifact])
            when (label == "runtime-demand-bundle")
                (checkOptimizedReferenceCases "acyclic runtime demand bundle" environment [artifact])
            when (label == "higher-kinded-stock-context")
                (checkOptimizedReferenceCases "higher-kinded stock context" environment [artifact])
            when (label `elem` ["higher-kinded-demands", "higher-kinded-polykinded-field",
                    "polymorphic-checked-payload-kinds", "polymorphic-dependent-payload-kinds",
                    "polymorphic-vacuous-quantification"])
                (checkOptimizedReferenceCases label environment [artifact])
            checkGeneratedArtifacts label artifact) artifacts
    reportFailure :: ReferenceCase -> IOException -> IO Bool
    reportFailure item exception
        -- FatalSetupFailure, asynchronous exceptions and filesystem/process IO
        -- failures propagate. Only ordinary assertion failures continue.
        | not (isUserError exception) = throwIO exception
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
