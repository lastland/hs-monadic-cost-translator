-- Compile clients against the real external package, with mtl explicitly exposed.
-- A missing package or an unrelated type error must not count as prohibition.
module RuntimeAllocator (checkRuntimeAllocator) where

import Control.Monad (forM_, unless)
import Data.List (isInfixOf, isSuffixOf, sort)
import Paths_cost_translate (getDataFileName)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, listDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeBaseName)
import System.IO.Temp (createTempDirectory)
import System.Process (readProcessWithExitCode)

checkRuntimeAllocator :: IO ()
checkRuntimeAllocator = do
    directory <- getDataFileName ("test" </> "runtime-allocator")
    clients <- filter (".hs" `isSuffixOf`) <$> listDirectory directory
    unless (sort clients == sort ("Control.hs" : map fst rejectedClients)) $
        fail ("allocator clients must all be registered: " ++ show clients)
    temporary <- getTemporaryDirectory
    root <- createTempDirectory temporary "cost-runtime-allocator-"
    putStrLn ("Allocator contract clients and logs: " ++ root)
    forM_ ["0", "2"] $ \optimization -> do
        source <- specimen "Control.hs"
        let output = root </> ("control-O" ++ optimization)
            executable = output </> "control"
        createDirectoryIfMissing True output
        (status, out, err) <- readProcessWithExitCode "ghc"
            (compilerFlags output ++ ["-O" ++ optimization, source, "-o", executable]) ""
        writeFile (output </> "compile.log") (out ++ err)
        unless (status == ExitSuccess) (fail ("allocator positive control did not compile\n" ++ out ++ err))
        (ran, results, problems) <- readProcessWithExitCode executable [] ""
        writeFile (output </> "execution.log") (results ++ problems)
        unless (ran == ExitSuccess && "PASS: ordinary State and native allocation" `isInfixOf` results) $
            fail ("allocator positive control failed\n" ++ results ++ problems)
    forM_ rejectedClients $ \(name, expected) -> do
        source <- specimen name
        let output = root </> takeBaseName name
        createDirectoryIfMissing True output
        (status, out, err) <- readProcessWithExitCode "ghc"
            (compilerFlags output ++ ["-fno-code", source]) ""
        let diagnostic = out ++ err
        writeFile (output </> "compile.log") diagnostic
        unless (status /= ExitSuccess && all (`isInfixOf` diagnostic) expected) $
            fail (name ++ ": allocator access must be rejected with " ++ show expected ++ ", got " ++
                show status ++ "\n" ++ diagnostic ++ "\nArtifacts: " ++ root)
    putStrLn ("PASS: runtime-allocator / " ++ show (length rejectedClients) ++
        " allocator-access clients rejected / ordinary State and native sharing preserved at O0 and O2")
  where
    specimen name = getDataFileName ("test" </> "runtime-allocator" </> name)

compilerFlags :: FilePath -> [String]
compilerFlags output =
    ["-v0", "-dynamic", "-Wall", "-Werror", "-fforce-recomp", "-fdiagnostics-color=never"
    ,"-hide-all-packages", "-package", "base", "-package", "MemoristANT", "-package", "mtl"
    ,"-i", "-outputdir", output]

rejectedClients :: [(FilePath, [String])]
rejectedClients =
    [(name, ["No instance for", "MonadState"]) |
        name <- ["Get.hs", "Put.hs", "Gets.hs", "Modify.hs", "ModifyStrict.hs", "State.hs",
                 "Alias.hs", "GenericWrapper.hs", "TransformerWrapper.hs", "Reset.hs"]] ++
    [("MemoristSuperclass.hs", ["Could not deduce", "MonadState"])
    ,("ConstructorReset.hs", ["Illegal term-level use", "M.MemoristT"])
    ,("DirectConstructor.hs", ["Illegal term-level use", "Runtime.MemoristT"])
    ,("RecordUpdate.hs", ["Not in scope: record field", "M.runMT"])
    ,("Coercion.hs", ["Couldn't match", "MemoristT"])
    ,("Enclose.hs", ["Not in scope", "Runtime.enclose"])
    ,("GenName.hs", ["Not in scope", "Runtime.genName"])
    ,("StrictGet.hs", ["No instance for", "MonadState"])
    ,("StrictConstructor.hs", ["Illegal term-level use", "Runtime.MemoristT"])
    ,("StrictRecordUpdate.hs", ["Not in scope: record field", "Runtime.runMT"])
    ,("StrictCoercion.hs", ["Couldn't match", "MemoristT"])
    ,("StrictGenName.hs", ["Not in scope", "Runtime.genName"])]
