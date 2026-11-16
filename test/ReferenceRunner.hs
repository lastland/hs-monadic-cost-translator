-- | One enforced execution path for every successful-translation fixture.
-- This module deliberately has no MemoristANT or QuickCheck build dependency.
module ReferenceRunner
    ( ReferenceEnvironment, Artifacts(..), prepareReferenceEnvironment
    , runReferenceCase, checkReferenceHarness
    ) where

import Control.Monad (forM, forM_, unless, when)
import Cost.Translate (translateFile)
import Paths_cost_translate (getDataFileName)
import System.Directory (copyFile, createDirectoryIfMissing, getTemporaryDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO.Temp (createTempDirectory)
import System.Process (readProcessWithExitCode)

data ReferenceEnvironment = ReferenceEnvironment
    { environmentRoot :: FilePath
    , supportDirectory :: FilePath
    , comparer :: FilePath
    }

data Artifacts = Artifacts
    { sourceFixture :: FilePath
    , generatedDirectory :: FilePath
    , referenceDirectory :: FilePath
    }

prepareReferenceEnvironment :: IO ReferenceEnvironment
prepareReferenceEnvironment = do
    temporary <- getTemporaryDirectory
    root <- createTempDirectory temporary "cost-reference-suite-"
    let support = root </> "support"
    createDirectoryIfMissing True support
    forM_ ["Protocol.hs", "ReferenceSupport.hs", "DemandSupport.hs", "Observe.hs", "Compare.hs"] $ \name -> do
        source <- getDataFileName ("test" </> "reference" </> name)
        copyFile source (support </> name)
    let executable = root </> "compare"
    compile support support "Compare" ["QuickCheck"] executable
    pure (ReferenceEnvironment root support executable)

runReferenceCase :: ReferenceEnvironment -> (String, [FilePath]) -> IO [Artifacts]
runReferenceCase environment (label, sources) = forM sources $ \fixture -> do
    root <- createTempDirectory (environmentRoot environment) (label ++ "-")
    putStrLn ("Reference fixtures and logs: " ++ root)
    let generated = root </> "generated"
        reference = root </> "reference"
    forM_ [generated, reference] $ \sandbox -> do
        createDirectoryIfMissing True sandbox
        stageSource fixture sandbox
        copyCase (label </> "Case.hs") (sandbox </> "Case.hs")
    result <- translateFile (generated </> "Example.hs") (generated </> "Generated.hs")
    either (fail . ((label ++ ": translation failed: ") ++)) pure result
    copyCase (label </> "Reference.hs") (reference </> "Generated.hs")
    actual <- buildObserver environment generated
    expected <- buildObserver environment reference
    (status, out, err) <- readProcessWithExitCode (comparer environment) [actual, expected] ""
    writeFile (root </> "comparison.log") (out ++ err)
    unless (status == ExitSuccess) $
        fail (label ++ ": reference comparison failed\n" ++ out ++ err ++ "\nArtifacts: " ++ root)
    putStr out
    pure (Artifacts fixture generated reference)

stageSource :: FilePath -> FilePath -> IO ()
stageSource fixture sandbox = do
    copyCase fixture (sandbox </> "Example.hs")
    -- Nonstandard fixture filenames can declare a different source module, e.g.
    -- Minimal. Keep that filename available to both identically compiled cases.
    when (takeFileName fixture /= "Example.hs") $
        copyCase fixture (sandbox </> takeFileName fixture)

copyCase :: FilePath -> FilePath -> IO ()
copyCase fixture destination = do
    source <- getDataFileName ("test" </> "cases" </> fixture)
    copyFile source destination

buildObserver :: ReferenceEnvironment -> FilePath -> IO FilePath
buildObserver environment sandbox = do
    let executable = sandbox </> "observe"
    compile (supportDirectory environment) sandbox "Observe" ["MemoristANT", "QuickCheck"] executable
    pure executable

compile :: FilePath -> FilePath -> String -> [String] -> FilePath -> IO ()
compile support sandbox entry packages executable = do
    let objects = sandbox </> (entry ++ "-objects")
    createDirectoryIfMissing True objects
    (status, out, err) <- readProcessWithExitCode "ghc"
        (["-v0", "-dynamic", "-Werror=unused-imports", "-i", "-i" ++ sandbox, "-i" ++ support,
          "-outputdir", objects, "-main-is", entry ++ ".main", support </> entry ++ ".hs",
          "-o", executable] ++ concatMap (\package -> ["-package", package]) packages) ""
    writeFile (sandbox </> "compile.log") (out ++ err)
    unless (status == ExitSuccess) (fail ("observer compilation failed in " ++ sandbox ++ "\n" ++ out ++ err))

-- These are infrastructure specimens, not translator correctness evidence.
-- They deliberately inject known mistakes and must be rejected by the same
-- controller and observer used for the real fixtures.
checkReferenceHarness :: ReferenceEnvironment -> IO ()
checkReferenceHarness environment = do
    root <- createTempDirectory (environmentRoot environment) "harness-mutations-"
    reference <- specimen root "Reference.hs"
    matching <- specimen root "Matching.hs"
    check root True "matching" matching reference
    forM_ ["WrongValue.hs", "ExtraTick.hs", "LostAlias.hs", "ExtraAllocation.hs", "PrematureForce.hs"] $ \name -> do
        mutant <- specimen root name
        check root False name mutant reference
    forM_ ["EmptyCase.hs", "SnapshotsOnly.hs", "FailuresOnly.hs", "BrokenProjection.hs"] $ \caseFile -> do
        actual <- specimenWithCase root "Matching.hs" caseFile
        expected <- specimenWithCase root "Reference.hs" caseFile
        check root False caseFile actual expected
    badSource <- getDataFileName ("test" </> "reference-selftest" </> "BadObserver.hs")
    let malformed = root </> "malformed-observer"
    compile (takeDirectory badSource) root "BadObserver" [] malformed
    check root False "malformed-response" malformed reference
    -- A comparer is not a worker: its usage error must not become a matching
    -- program exception. This also exercises worker exit/protocol supervision.
    check root False "wrong-executable" (comparer environment) reference
    demandChecks <- getDataFileName ("test" </> "reference-selftest" </> "DemandChecks.hs")
    let demandExecutable = root </> "demand-checks"
    compile (takeDirectory demandChecks) (supportDirectory environment) "DemandChecks"
        ["MemoristANT", "QuickCheck"] demandExecutable
    (demandStatus, demandOut, demandErr) <- readProcessWithExitCode demandExecutable [] ""
    writeFile (root </> "demand-checks.log") (demandOut ++ demandErr)
    unless (demandStatus == ExitSuccess) (fail (demandOut ++ demandErr))
    putStrLn "PASS: reference-harness / values, ticks, aliases, allocations, laziness, worker failures, and demand codecs"
  where
    specimen root name = specimenWithCase root name "Case.hs"
    specimenWithCase root name caseFile = do
        let sandbox = root </> (name ++ "-" ++ caseFile)
        createDirectoryIfMissing True sandbox
        forM_ [(name, "Generated.hs"), (caseFile, "Case.hs")] $ \(sourceName, destinationName) -> do
            source <- getDataFileName ("test" </> "reference-selftest" </> sourceName)
            copyFile source (sandbox </> destinationName)
        buildObserver environment sandbox
    check root shouldPass label actual expected = do
        (status, out, err) <- readProcessWithExitCode (comparer environment) [actual, expected] ""
        writeFile (root </> "mutation.log") (label ++ "\n" ++ out ++ err)
        unless ((status == ExitSuccess) == shouldPass) $
            fail ("reference harness did not detect " ++ label ++ "\n" ++ out ++ err)
