-- | One enforced execution path for every successful-translation fixture.
-- This module deliberately has no MemoristANT or QuickCheck build dependency.
module ReferenceRunner
    ( ReferenceEnvironment, Artifacts(..), prepareReferenceEnvironment
    , runReferenceCase, checkReferenceHarness, checkGeneratedFunctionLifecycle
    , checkGeneratedConstructionLifecycle
    ) where

import Control.Monad (forM, forM_, unless, when)
import Cost.Translate (translateFile)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (fromMaybe)
import GeneratedConstruction (checkGeneratedConstruction)
import GeneratedFunctions (checkTopLevelFunctions)
import Paths_cost_translate (getDataFileName)
import System.Directory
    ( Permissions(..), copyFile, createDirectoryIfMissing, doesFileExist, findExecutable
    , getPermissions, getTemporaryDirectory, setPermissions
    )
import System.Environment (getEnvironment, getExecutablePath)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO.Temp (createTempDirectory)
import System.Process (CreateProcess(env), proc, readCreateProcessWithExitCode, readProcessWithExitCode)

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
    checkTopLevelFunctions (generated </> "Example.hs") (generated </> "Generated.hs")
    checkGeneratedConstruction (generated </> "Generated.hs")
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
    source <- getDataFileName fixture
    copyFile source (sandbox </> "Example.hs")
    -- Nonstandard fixture filenames can declare a different source module, e.g.
    -- Minimal. Keep that filename available to both identically compiled cases.
    when (takeFileName fixture /= "Example.hs") $
        copyFile source (sandbox </> takeFileName fixture)

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

-- Mutate a real publication, not the lifecycle API. The wrapper first runs real
-- GHC, then replaces its successfully checked candidate before publication.
-- The function mutant loses its export; the construction mutant keeps identity's
-- public type, values and costs. Neither may reach caller compilation.
checkGeneratedFunctionLifecycle :: ReferenceEnvironment -> IO ()
checkGeneratedFunctionLifecycle environment = do
    checkPublicationGuard environment "function" ("generated-functions" </> "Missing.hs")
        ["Example.hs:2:", "top-level source function f", "module-level generated binding fM"]
    putStrLn "PASS: generated-functions / ordinary fixture lifecycle rejects corrupted publication before caller compilation"

checkGeneratedConstructionLifecycle :: ReferenceEnvironment -> IO ()
checkGeneratedConstructionLifecycle environment = do
    checkPublicationGuard environment "construction" ("generated-construction" </> "ReasoningPublication.hs")
        ["Generated.hs:8:", "unapproved runtime operation Memorist.Base.Core.value",
          "unapproved runtime operation Memorist.Base.Core.eval1", "is forbidden in generated code"]
    checkPublicationGuard environment "construction-unused" ("generated-construction" </> "UnusedNamingPublication.hs")
        ["Generated.hs:9:", "unapproved runtime operation Memorist.Base.Thunk.nnext", "is forbidden in generated code"]
    checkPublicationGuard environment "construction-carrier" ("generated-construction" </> "UnusedCarrierPublication.hs")
        ["Generated.hs:9:", "unapproved selected method GHC.Internal.Base.pure", "audited Memorist instance"]
    putStrLn "PASS: generated-construction / ordinary fixture lifecycle rejects reasoning operations, unused naming helpers and a wrong selected carrier before caller compilation"

checkPublicationGuard :: ReferenceEnvironment -> String -> FilePath -> [String] -> IO ()
checkPublicationGuard environment label specimen expected = do
    root <- createTempDirectory (environmentRoot environment) (label ++ "-lifecycle-")
    let commands = root </> "commands"
        wrapper = commands </> "ghc"
        marker = root </> "mutation.log"
    createDirectoryIfMissing True commands
    realGhc <- findExecutable "ghc" >>= maybe (fail "lifecycle specimen requires ghc") pure
    wrapperSource <- getDataFileName ("test" </> "generated-functions" </> "PublishMissing.sh")
    replacement <- getDataFileName ("test" </> specimen)
    copyFile wrapperSource wrapper
    permissions <- getPermissions wrapper
    setPermissions wrapper permissions { executable = True }
    executablePath <- getExecutablePath
    processEnvironment <- getEnvironment
    let injected =
            [("PATH", commands ++ ":" ++ fromMaybe "" (lookup "PATH" processEnvironment))
            ,("TMPDIR", root), ("FUNCTION_SPECIMEN_GHC", realGhc)
             ,("FUNCTION_SPECIMEN_OUTPUT", replacement), ("FUNCTION_SPECIMEN_LOG", marker)]
        child = (proc executablePath ["identity"])
            { env = Just (injected ++ filter (\(key, _) -> key `notElem` map fst injected) processEnvironment) }
    (status, out, err) <- readCreateProcessWithExitCode child ""
    writeFile (root </> "lifecycle.log") (out ++ err)
    mutated <- doesFileExist marker
    fixture <- case [drop (length prefix) line | line <- lines out, prefix `isPrefixOf` line] of
        [directory] -> pure directory
        _ -> fail ("lifecycle specimen did not reach the ordinary fixture\n" ++ out ++ err)
    let generated = fixture </> "generated"
    published <- doesFileExist (generated </> "Generated.hs")
    compiled <- doesFileExist (generated </> "compile.log")
    unless (status /= ExitSuccess && mutated && published && not compiled &&
            all (`isInfixOf` err) ((generated </> "Generated.hs") : expected)) $
        fail ("global " ++ label ++ " guard did not reject a corrupted ordinary fixture before caller compilation\n" ++
              out ++ err ++ "\nArtifacts: " ++ root)
  where
    prefix = "Reference fixtures and logs: "

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
