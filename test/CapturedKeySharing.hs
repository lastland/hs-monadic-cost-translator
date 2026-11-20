-- Local sharing evidence is deliberately separate from the whole-publication
-- construction gate. Open quot and common support still block that gate.
module CapturedKeySharing (checkCapturedKeySharing) where

import ConstructionAst (collectAst, pretty)
import Control.Exception (IOException, try)
import Control.Monad (forM_, unless)
import Cost.Translate (translateFile)
import Data.List (isInfixOf)
import GeneratedConstruction (checkGeneratedConstruction)
import GeneratedFunctions (checkTopLevelFunctions, parseHaskellModule)
import GHC
import Paths_cost_translate (getDataFileName)
import ReferenceRunner (ReferenceEnvironment, comparer)
import System.Directory (copyFile, createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import System.Process (readProcessWithExitCode)

checkCapturedKeySharing :: ReferenceEnvironment -> IO ()
checkCapturedKeySharing environment = do
    root <- createTempDirectory ".scratch" "captured-key-"
    putStrLn ("Captured key artifacts: " ++ root)
    fixtures <- getDataFileName "test/cases/inferred-key-captured"
    support <- getDataFileName "test/reference"
    let actual = root </> "actual"
        reference = root </> "reference"
        output = actual </> "Generated.hs"
    forM_ [actual, reference] $ \sandbox -> do
        createDirectoryIfMissing True sandbox
        forM_ ["Example.hs", "Case.hs"] $ \file ->
            copyFile (fixtures </> file) (sandbox </> file)
    translateFile (actual </> "Example.hs") output >>= either fail pure
    checkTopLevelFunctions (actual </> "Example.hs") output
    ast <- parseHaskellModule output
    let owners = [b | L _ (ValD _ b@(FunBind _ name _)) <- hsmodDecls ast,
                     pretty (unLoc name) == "viewsM"]
        calls = [pretty (unLoc name) | b <- owners, HsVar _ name <- collectAst b :: [HsExpr GhcPs]]
    unless (length owners == 1 && "mapThunk" `elem` calls &&
            not (any (`elem` calls) ["sharedAt", "sharedAtBy", "M.eval", "M.value"])) $
        fail "captured producer must use the inferred native root and structural views, not the legacy evaluator"
    blocked <- try (checkGeneratedConstruction output) :: IO (Either IOException ())
    case blocked of
        Left diagnostic | all (`isInfixOf` show diagnostic) ["quot", "tval"] ->
            writeFile (root </> "whole-publication-blocked.log") (show diagnostic)
        _ -> fail ("whole-publication construction guard must retain its independent blockers: " ++ show blocked)
    copyFile (fixtures </> "Reference.hs") (reference </> "Generated.hs")
    forM_ ["-O0", "-O2"] $ \optimization -> do
        observed <- build support actual "Observe" ("observe" ++ optimization) ["MemoristANT", "QuickCheck"] optimization
        expected <- build support reference "Observe" ("observe" ++ optimization) ["MemoristANT", "QuickCheck"] optimization
        (status, out, err) <- readProcessWithExitCode (comparer environment) [observed, expected] ""
        writeFile (root </> ("comparison" ++ optimization ++ ".log")) (out ++ err)
        unless (status == ExitSuccess) (fail (out ++ err))
    putStrLn "PASS: captured-key-sharing / local native sharing at O0/O2; whole publication remains blocked"

build :: FilePath -> FilePath -> String -> String -> [String] -> String -> IO FilePath
build support sandbox entry filename packages optimization = do
    let output = sandbox </> filename
        objects = sandbox </> (filename ++ "-objects")
    createDirectoryIfMissing True objects
    (status, out, err) <- readProcessWithExitCode "ghc"
        (["-v0", "-dynamic", optimization, "-Werror=unused-imports", "-i", "-i" ++ sandbox, "-i" ++ support,
          "-outputdir", objects, "-main-is", entry ++ ".main", support </> (entry ++ ".hs"), "-o", output] ++
         concatMap (\package -> ["-package", package]) packages) ""
    writeFile (output ++ ".compile.log") (out ++ err)
    unless (status == ExitSuccess) (fail (out ++ err))
    pure output
