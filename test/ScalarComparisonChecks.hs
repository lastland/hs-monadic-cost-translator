module ScalarComparisonChecks (checkScalarComparisonCallers) where

import Control.Monad (forM_, unless)
import ReferenceRunner (Artifacts(..), ReferenceEnvironment, buildObserverAt, comparer)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

-- The ordinary lifecycle already checked the whole publication and compared
-- it at the default optimization. Rebuild the same caller/reference artifacts,
-- without changing their probes or the handwritten value/cost oracle.
checkScalarComparisonCallers :: ReferenceEnvironment -> [Artifacts] -> IO ()
checkScalarComparisonCallers environment artifacts = forM_ artifacts $ \artifact ->
    forM_ ["-O0", "-O2"] $ \optimization -> do
        actual <- buildObserverAt optimization environment (generatedDirectory artifact)
        expected <- buildObserverAt optimization environment (referenceDirectory artifact)
        (status, out, err) <- readProcessWithExitCode (comparer environment) [actual, expected] ""
        writeFile (generatedDirectory artifact </> ("comparison" ++ optimization ++ ".log")) (out ++ err)
        unless (status == ExitSuccess) (fail (optimization ++ ": " ++ out ++ err))
        putStrLn ("PASS: caller scalar comparisons / " ++ optimization ++ " / boundary cases and 200 randomized value/cost comparisons")
