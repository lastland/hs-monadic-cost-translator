-- | Public lifecycle for one whole-module translation.
--
-- 'translateFile' runs, in order:
--
-- 1. Generate the translated module text (see "Cost.Translate.Frontend").
-- 2. Write @OUTPUT.hs.log@: resolved identities, arities, and diagnostics.
-- 3. Typecheck the candidate with an external
--    @ghc -fno-code -package MemoristANT -package QuickCheck@, from a
--    temporary directory next to the requested output.
-- 4. Publish by renaming the candidate over the output — on success only.
--
-- GHC failures are categorized as @setup error: target modules are
-- unavailable in GHC's environment@ (the target packages cannot be found)
-- or @generated-code type error@ (a translator defect); the failed
-- candidate's text is appended to the log. Failure never touches the output
-- file, so an older successful output is never clobbered — only the log
-- reveals what happened.
module Cost.Translate (translateFile) where

import Control.Exception (SomeException, displayException, try)
import Cost.Translate.Frontend (generate)
import Cost.Translate.Diagnostic (renderDiagnostic)
import System.Directory (renameFile)
import System.Exit (ExitCode(..))
import System.FilePath (takeBaseName, takeDirectory, (</>))
import System.IO.Temp (withTempDirectory)
import System.Process (readProcessWithExitCode)
import Data.List (isInfixOf)

translateFile :: FilePath -> FilePath -> IO (Either String ())
translateFile source output = do
    result <- try (generate source (takeBaseName output))
    case result of
        Left (e :: SomeException) -> failure ("source error: " ++ displayException e)
        Right (Left diagnostic, logText) -> failure (logText ++ renderDiagnostic diagnostic)
        Right (Right code, logText) -> do
            writeFile logPath logText
            withTempDirectory (takeDirectory output) ".cost-translate-" $ \temp -> do
                let candidate = temp </> takeBaseName output ++ ".hs"
                writeFile candidate code
                (status, out, err) <- readProcessWithExitCode "ghc"
                    ["-v0", "-fno-code", "-fforce-recomp", "-i" ++ takeDirectory source, "-package", "MemoristANT", "-package", "QuickCheck", candidate] ""
                appendFile logPath (out ++ err)
                case status of
                    ExitSuccess -> do
                        renameFile candidate output
                        appendFile logPath "Generated module typechecked; published output\n"
                        pure (Right ())
                    ExitFailure _ -> do
                        appendFile logPath ("Failed candidate:\n" ++ code)
                        let category = if any (`isInfixOf` (out ++ err)) ["cannot satisfy -package", "Could not find module", "Could not load module"]
                                then "setup error: target modules are unavailable in GHC's environment"
                                else "generated-code type error"
                        appendFile logPath (category ++ "\n")
                        pure (Left (category ++ "; see " ++ logPath))
  where
    logPath = output ++ ".log"
    failure message = writeFile logPath (message ++ "\n") >> pure (Left message)
