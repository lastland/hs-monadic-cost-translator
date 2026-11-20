-- | One enforced execution path for every successful-translation fixture.
-- This module deliberately has no MemoristANT or QuickCheck build dependency.
module ReferenceRunner
    ( ReferenceEnvironment, Artifacts(..), prepareReferenceEnvironment
    , runReferenceCase, checkReferenceHarness, checkGeneratedFunctionLifecycle
    , checkGeneratedConstructionLifecycle
    , checkSourceScopeLifecycle
    , buildObserver, buildObserverAt, comparer, checkOptimizedReferenceCases
    , retainReferenceFailure
    ) where

import Control.Exception (IOException, catch, throwIO, try, bracket_)
import Control.Monad (forM, forM_, unless, when, void)
import AcceptedOptionsChecks (checkAcceptedOptions)
import Cost.Publication (stageAndTranslate)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (fromMaybe)
import GeneratedConstruction (checkGeneratedConstruction, checkGeneratedConstructionWithOrigin)
import GeneratedFunctions (checkAcceptedFunctions)
import Paths_cost_translate (getDataFileName)
import System.Directory
    ( Permissions(..), copyFile, createDirectoryIfMissing, doesFileExist, findExecutable
    , getPermissions, getTemporaryDirectory, setPermissions
    )
import System.Environment (getEnvironment, getExecutablePath, setEnv, unsetEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory, takeFileName, isAbsolute)
import System.IO.Temp (createTempDirectory)
import System.IO.Error (ioeGetErrorString, ioeSetErrorString)
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
    forM_ ["ghc", "hlint"] $ \command ->
        findExecutable command >>= maybe (fail ("reference setup error: missing " ++ command)) (const (pure ()))
    (runtimeStatus, runtimeOut, runtimeErr) <- readProcessWithExitCode "ghc-pkg" ["latest", "MemoristANT"] ""
    unless (runtimeStatus == ExitSuccess) $
        fail ("reference setup error: runtime unavailable\n" ++ runtimeOut ++ runtimeErr)
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
    retainReferenceFailure root $ do
        let generated = root </> "generated"
            reference = root </> "reference"
        forM_ [generated, reference] $ \sandbox -> do
            createDirectoryIfMissing True sandbox
            when (sandbox == reference) (stageSource fixture sandbox)
            copyCase (label </> "Case.hs") (sandbox </> "Case.hs")
        source <- fixturePath fixture
        result <- stageAndTranslate source (generated </> "Generated.hs")
        publication <- either (fail . ((label ++ ": translation failed: ") ++)) pure result
        checkAcceptedFunctions publication (generated </> "Generated.hs")
        checkGeneratedConstructionWithOrigin publication (generated </> "Generated.hs")
        copyCase (label </> "Reference.hs") (reference </> "Generated.hs")
        actual <- buildObserver environment generated
        expected <- buildObserver environment reference
        (status, out, err) <- readProcessWithExitCode (comparer environment) [actual, expected] ""
        writeFile (root </> "comparison.log") (out ++ err)
        unless (status == ExitSuccess) $
            fail (label ++ ": reference comparison failed\n" ++ out ++ err)
        putStr out
        pure (Artifacts fixture generated reference)

retainReferenceFailure :: FilePath -> IO a -> IO a
retainReferenceFailure root action = action `catch` retain
  where
    retain :: IOException -> IO a
    retain exception = do
        let message = ioeGetErrorString exception ++ "\nArtifacts: " ++ root ++
                "\nFailure log: " ++ root </> "failure.log"
        writeFile (root </> "failure.log") message
        throwIO (ioeSetErrorString exception message)

stageSource :: FilePath -> FilePath -> IO ()
stageSource fixture sandbox = do
    source <- fixturePath fixture
    copyFile source (sandbox </> "Example.hs")
    -- Nonstandard fixture filenames can declare a different source module, e.g.
    -- Minimal. Keep that filename available to both identically compiled cases.
    when (takeFileName fixture /= "Example.hs") $
        copyFile source (sandbox </> takeFileName fixture)

fixturePath :: FilePath -> IO FilePath
fixturePath file = if isAbsolute file then pure file else getDataFileName file

copyCase :: FilePath -> FilePath -> IO ()
copyCase fixture destination = do
    source <- getDataFileName ("test" </> "cases" </> fixture)
    copyFile source destination

buildObserver :: ReferenceEnvironment -> FilePath -> IO FilePath
buildObserver = buildObserverWith []

buildObserverAt :: String -> ReferenceEnvironment -> FilePath -> IO FilePath
buildObserverAt optimization = buildObserverWith [optimization]

-- Rebuild the same ordinary lifecycle artifacts without changing their probes,
-- generated publication, or independent handwritten value/cost oracle.
checkOptimizedReferenceCases :: String -> ReferenceEnvironment -> [Artifacts] -> IO ()
checkOptimizedReferenceCases label environment artifacts = forM_ artifacts $ \artifact ->
    forM_ ["-O0", "-O2"] $ \optimization -> do
        actual <- buildObserverAt optimization environment (generatedDirectory artifact)
        expected <- buildObserverAt optimization environment (referenceDirectory artifact)
        (status, out, err) <- readProcessWithExitCode (comparer environment) [actual, expected] ""
        writeFile (generatedDirectory artifact </> ("comparison" ++ optimization ++ ".log")) (out ++ err)
        unless (status == ExitSuccess) (fail (optimization ++ ": " ++ out ++ err))
        putStrLn ("PASS: " ++ label ++ " / " ++ optimization ++ " / boundary cases and 200 randomized value/cost comparisons")

buildObserverWith :: [String] -> ReferenceEnvironment -> FilePath -> IO FilePath
buildObserverWith options environment sandbox = do
    let executable = sandbox </> ("observe" ++ concat options)
    compileAt options (supportDirectory environment) sandbox "Observe"
        ["MemoristANT", "QuickCheck"] executable
    pure executable

compile :: FilePath -> FilePath -> String -> [String] -> FilePath -> IO ()
compile = compileAt []

compileAt :: [String] -> FilePath -> FilePath -> String -> [String] -> FilePath -> IO ()
compileAt options support sandbox entry packages executable = do
    let objects = sandbox </> (entry ++ concat options ++ "-objects")
    createDirectoryIfMissing True objects
    (status, out, err) <- readProcessWithExitCode "ghc"
        (options ++ ["-v0", "-dynamic", "-Werror=unused-imports", "-i", "-i" ++ sandbox, "-i" ++ support,
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
checkPublicationGuard environment = checkPublicationGuardFor environment "identity"

checkPublicationGuardFor :: ReferenceEnvironment -> String -> String -> FilePath -> [String] -> IO ()
checkPublicationGuardFor environment selector label specimen =
    checkPublicationGuardUsing environment selector label specimen "PublishMissing.sh" []

checkPublicationGuardUsing :: ReferenceEnvironment -> String -> String -> FilePath -> String -> [(String, String)] -> [String] -> IO ()
checkPublicationGuardUsing environment selector label specimen wrapperName extraEnvironment expected = do
    root <- createTempDirectory (environmentRoot environment) (label ++ "-lifecycle-")
    let commands = root </> "commands"
        wrapper = commands </> "ghc"
        marker = root </> "mutation.log"
    createDirectoryIfMissing True commands
    realGhc <- findExecutable "ghc" >>= maybe (fail "lifecycle specimen requires ghc") pure
    wrapperSource <- getDataFileName ("test" </> "generated-functions" </> wrapperName)
    replacement <- getDataFileName ("test" </> specimen)
    copyFile wrapperSource wrapper
    permissions <- getPermissions wrapper
    setPermissions wrapper permissions { executable = True }
    executablePath <- getExecutablePath
    processEnvironment <- getEnvironment
    let injected = extraEnvironment ++
            [("PATH", commands ++ ":" ++ fromMaybe "" (lookup "PATH" processEnvironment))
            ,("TMPDIR", root), ("FUNCTION_SPECIMEN_GHC", realGhc)
             ,("FUNCTION_SPECIMEN_OUTPUT", replacement), ("FUNCTION_SPECIMEN_LOG", marker)]
        child = (proc executablePath [selector])
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
    when (label `elem` ["origin-term", "origin-helper", "origin-section"] &&
            (generated </> "Example.hs:") `isInfixOf` err) $
        fail ("origin uses must not restore original-body obligations\n" ++ err)
    when (label == "origin-term") $
        unless (length (filter ("ordinary originating-source term reference " `isInfixOf`) (lines err)) == 15 &&
                not ("unapproved " `isInfixOf` err)) $
            fail ("expected exactly fifteen term uses, not structural field labels or original-body reports\n" ++ err)
    when (label == "origin-section") $
        unless (length (filter ("ordinary originating-source term reference " `isInfixOf`) (lines err)) == 4 &&
                length (filter ("colliding-origin-section.hs:100:" `isInfixOf`) (lines err)) == 2 &&
                not ("unapproved " `isInfixOf` err)) $
            fail ("section diagnostics must count physical written uses, including LINE collisions\n" ++ err)
  where
    prefix = "Reference fixtures and logs: "

checkSourceScopeLifecycle :: ReferenceEnvironment -> IO ()
checkSourceScopeLifecycle environment = do
    checkAcceptedOptions
    checkPackageEnvironmentLifecycle environment
    artifacts <- runReferenceCase environment ("source-map", ["test/cases/source-map/Example.hs"])
    forM_ artifacts $ \artifact -> do
        result <- try (checkGeneratedConstruction (generatedDirectory artifact </> "Generated.hs"))
        case result of
            Left (err :: IOException) ->
                unless ("Example.hs:9:" `isInfixOf` ioeGetErrorString err &&
                    "GHC.Internal.Base.map" `isInfixOf` ioeGetErrorString err)
                    (fail ("strict standalone rejected for the wrong reason: " ++ show err))
            Right () -> fail "standalone construction checking must remain strict"
    manual <- runReferenceCase environment ("manual-thunk-arguments", ["examples/ThunkArguments.hs"])
    checkSourceOptimizations environment (artifacts ++ manual)
    checkPublicationGuardFor environment "source-map" "origin-term"
        ("generated-construction" </> "OriginTermPublication.hs")
        ("ordinary originating-source term reference Example.keep" :
         "ordinary originating-source term reference Example.partial" :
         "ordinary originating-source term reference Example.payload" :
         "ordinary originating-source term reference Example.zero" :
         [ "Generated.hs:" ++ show line ++ ":"
         | line <- [10, 13, 16, 19, 25, 28, 31, 37, 43, 46, 49, 58, 61, 67, 73 :: Int] ])
    forM_ [("snapshot", "accepted origin snapshot mismatch"),
           ("options", "accepted origin snapshot mismatch"),
           ("path", "origin loaded from an unrecorded path")] $ \(mutation, diagnostic) ->
        checkPublicationGuardUsing environment "source-map" ("origin-" ++ mutation)
            ("generated-construction" </> "OriginTermPublication.hs")
            "PublishOriginMutation.sh" [("ORIGIN_SPECIMEN_MUTATION", mutation)] [diagnostic]
    helper <- getDataFileName ("test" </> "generated-construction" </> "OriginHelper.hs")
    checkPublicationGuardUsing environment "source-map" "origin-helper"
        ("generated-construction" </> "OriginHelperPublication.hs")
        "PublishOriginMutation.sh"
        [("ORIGIN_SPECIMEN_MUTATION", "helper"), ("ORIGIN_SPECIMEN_HELPER", helper)]
        ["Generated.hs:14:", "OriginHelper.hs:10:", "GHC.Internal.Base.map",
         "OriginHelper.hs:13:", "Memorist.Base.Thunk.tval",
         "OriginHelper.hs:16:", "OriginHelper.hs:19:",
         "ordinary originating-source term reference Example.keep",
         "ordinary originating-source term reference Example.partial",
         "OriginHelper.hs:22:", "OriginHelper.hs:25:", "OriginHelper.hs:31:",
         "OriginHelper.hs:37:", "OriginHelper.hs:43:"]
    checkChangedInclude environment
    checkAcceptedCPP environment
    checkPublicationGuardFor environment "source-map" "origin-section"
        ("generated-construction" </> "OriginSectionPublication.hs")
        ["Generated.hs:13:", "Generated.hs:16:", "colliding-origin-section.hs:100:",
         "ordinary originating-source term reference Example.keep",
         "ordinary originating-source term reference Example.partial"]
    putStrLn "PASS: source-scope / real translation, strict standalone, O0/O2 references and live/dead/callback/helper/method source-use rejection"

checkSourceOptimizations :: ReferenceEnvironment -> [Artifacts] -> IO ()
checkSourceOptimizations environment artifacts = forM_ artifacts $ \artifact ->
    forM_ ["-O0", "-O2"] $ \optimization -> do
        actual <- buildObserverAt optimization environment (generatedDirectory artifact)
        expected <- buildObserverAt optimization environment (referenceDirectory artifact)
        (status, out, err) <- readProcessWithExitCode (comparer environment) [actual, expected] ""
        unless (status == ExitSuccess) (fail (out ++ err))

checkPackageEnvironmentLifecycle :: ReferenceEnvironment -> IO ()
checkPackageEnvironmentLifecycle environment = do
    root <- createTempDirectory (environmentRoot environment) "origin-package-environment-"
    let packageEnvironment = root </> "packages"
    (status, out, err) <- readProcessWithExitCode "ghc-pkg" ["field", "base", "id", "--simple-output"] ""
    unless (status == ExitSuccess && length (words out) == 1)
        (fail ("cannot identify the actual base package for the lifecycle: " ++ err))
    writeFile packageEnvironment ("package-id " ++ unwords (words out) ++ "\n")
    previous <- lookup "GHC_ENVIRONMENT" <$> getEnvironment
    bracket_ (setEnv "GHC_ENVIRONMENT" packageEnvironment)
        (maybe (unsetEnv "GHC_ENVIRONMENT") (setEnv "GHC_ENVIRONMENT") previous) $
        void (runReferenceCase environment ("source-map", ["test/cases/source-map/Example.hs"]))

checkChangedInclude :: ReferenceEnvironment -> IO ()
checkChangedInclude environment = do
    root <- createTempDirectory (environmentRoot environment) "origin-include-"
    let wrapper = root </> "ghc"
        marker = root </> "mutation.log"
        header = root </> "include.h"
        fixture = root </> "OriginCPP.hs"
    template <- getDataFileName "test/generated-construction/OriginCPP.hs" >>= readFile
    let original = unlines [if line == "#include \"ORIGIN_INCLUDE\""
            then "#include " ++ show header else line | line <- lines template]
    writeFile fixture original
    writeFile header "#define IDENTITY(x) x\n"
    realGhc <- findExecutable "ghc" >>= maybe (fail "include control requires GHC") pure
    wrapperSource <- getDataFileName "test/generated-functions/PublishOriginMutation.sh"
    copyFile wrapperSource wrapper
    permissions <- getPermissions wrapper
    setPermissions wrapper permissions { executable = True }
    previous <- getEnvironment
    let injected =
            [("PATH", root ++ ":" ++ fromMaybe "" (lookup "PATH" previous)),
             ("FUNCTION_SPECIMEN_GHC", realGhc), ("FUNCTION_SPECIMEN_LOG", marker),
             ("ORIGIN_SPECIMEN_MUTATION", "include"), ("ORIGIN_SPECIMEN_INCLUDE", header)]
        restore (key, _) = maybe (unsetEnv key) (setEnv key) (lookup key previous)
    result <- bracket_ (mapM_ (uncurry setEnv) injected) (mapM_ restore injected) $
        try (runReferenceCase environment ("source-map", [fixture]))
    case result of
        Left (err :: IOException) ->
            unless ("accepted origin snapshot mismatch: frontend buffer" `isInfixOf` ioeGetErrorString err)
                (fail ("changed include rejected for the wrong reason: " ++ show err))
        Right _ -> fail "changed CPP inputs must not inherit original-source authority"
    candidate <- readFile marker
    sandbox <- case lines candidate of
        [path] -> pure (takeDirectory (takeDirectory path))
        _ -> fail "include control did not typecheck exactly one real candidate"
    staged <- readFile (sandbox </> "Example.hs")
    headerAfter <- readFile header
    compiled <- doesFileExist (sandbox </> "compile.log")
    unless (original == staged && headerAfter == "#define IDENTITY(x) 0\n" && not compiled)
        (fail "changed-include control must preserve raw source bytes and stop before callers")
    strict <- try (checkGeneratedConstruction (sandbox </> "Generated.hs"))
    case strict of
        Left (err :: IOException) ->
            unless ("preprocessed source" `isInfixOf` ioeGetErrorString err)
                (fail ("construction CPP boundary rejected for the wrong reason: " ++ show err))
        Right () -> fail "construction checking must not accept preprocessed home sources"

checkAcceptedCPP :: ReferenceEnvironment -> IO ()
checkAcceptedCPP environment = do
    root <- createTempDirectory (environmentRoot environment) "accepted-cpp-"
    let header = root </> "include.h"
        included = root </> "IncludedCPP.hs"
        identity = root </> "IdentityCPP.hs"
    writeFile header "#define IDENTITY(x) x\n"
    writeFile included (unlines
        ["{-# LANGUAGE CPP #-}", "module Example where",
         "#include " ++ show header, "f :: a -> a", "f x = IDENTITY(x)"])
    writeFile identity (unlines
        ["{-# LANGUAGE CPP #-}", "module Example where",
         "#define IDENTITY(x) x", "#define HIDDEN hidden",
         "f :: a -> a", "f x = IDENTITY(x)",
         "HIDDEN :: a -> a", "HIDDEN x = x",
         "(%%) :: a -> b -> a", "x %% y = x"])
    artifacts <- runReferenceCase environment ("identity", [included])
    pureArtifacts <- runReferenceCase environment ("identity", [identity])
    checkSourceOptimizations environment (artifacts ++ pureArtifacts)
    let pureOutput = root </> "PureGenerated.hs"
    purePublication <- stageAndTranslate identity pureOutput >>= either fail pure
    writeFile (root </> "Example.hs") "module Example where\n"
    writeFile pureOutput "module PureGenerated where\n"
    missing <- try (checkAcceptedFunctions purePublication pureOutput)
    case missing of
        Left (err :: IOException) ->
            unless (all (`isInfixOf` ioeGetErrorString err)
                    ["binding fM", "binding hiddenM", "binding operatorM_37_37"])
                (fail ("accepted CPP syntax lost original obligations: " ++ show err))
        Right () -> fail "accepted CPP syntax must retain hidden and operator obligations"
    putStrLn "PASS: pure accepted CPP / actual parsed syntax, hidden/operator obligations / independent O0/O2 references"

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
