module Cost.Publication
    ( translateFile
    , generatedName
    , Publication
    , stageAndTranslate
    , verifyOrigin
    ) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (void)
import Cost.AcceptedSource
import Cost.Translate.Diagnostic (renderDiagnostic)
import Cost.Translate.Frontend (generate)
import Cost.Translate.Names (generatedName)
import Data.List (isInfixOf)
import GHC qualified
import System.Directory (canonicalizePath, copyFile, renameFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName, takeDirectory, takeFileName, (</>))
import System.IO.Temp (withTempDirectory)
import System.Process (readProcessWithExitCode)

translateFile :: FilePath -> FilePath -> IO (Either String ())
translateFile source output =
    fmap void (translateAccepted source output)

translateAccepted :: FilePath -> FilePath -> IO (Either String AcceptedSource)
translateAccepted source output = do
    result <- try (generate source (takeBaseName output))
    case result of
        Left (e :: SomeException) ->
            failure ("source error: " ++ displayException e)
        Right (Left diagnostic, logText) ->
            failure (logText ++ renderDiagnostic diagnostic)
        Right (Right (code, accepted), logText) -> do
            writeFile logPath logText
            published <-
                withTempDirectory (takeDirectory output) ".cost-translate-" $
                    publishCandidate source output code
            pure (accepted <$ published)
    where
        logPath = output ++ ".log"
        failure message =
            writeFile logPath (message ++ "\n") >> pure (Left message)

data Publication = Publication FilePath [FilePath] AcceptedSource

stageAndTranslate :: FilePath -> FilePath -> IO (Either String Publication)
stageAndTranslate fixture output = do
    let source = takeDirectory output </> "Example.hs"
        companion = takeDirectory output </> takeFileName fixture
        paths = if source == companion then [source] else [source, companion]
    mapM_ (copyFile fixture) paths
    physical <- mapM canonicalizePath paths
    result <- translateAccepted source output
    published <- canonicalizePath output
    pure (Publication published physical <$> result)

verifyOrigin :: Publication -> FilePath -> [GHC.ModSummary] -> IO [GHC.Module]
verifyOrigin (Publication published paths accepted) output summaries = do
    actual <- canonicalizePath output
    if actual /= published
        then fail "origin publication path mismatch"
        else verifyAccepted paths accepted summaries

publishCandidate ::
    FilePath -> FilePath -> String -> FilePath -> IO (Either String ())
publishCandidate source output code temp = do
    let candidate = temp </> takeBaseName output ++ ".hs"
    writeFile candidate code
    (status, out, err) <-
        readProcessWithExitCode
            "ghc"
            [ "-v0"
            , "-fno-code"
            , "-fforce-recomp"
            , "-i" ++ takeDirectory source
            , "-package"
            , "MemoristANT"
            , "-package"
            , "QuickCheck"
            , candidate
            ]
            ""
    appendFile logPath (out ++ err)
    case status of
        ExitSuccess -> do
            renameFile candidate output
            appendFile
                logPath
                "Generated module typechecked; published output\n"
            pure (Right ())
        ExitFailure _ -> do
            appendFile logPath ("Failed candidate:\n" ++ code)
            let category = failureCategory (out ++ err)
            appendFile logPath (category ++ "\n")
            pure (Left (category ++ "; see " ++ logPath))
    where
        logPath = output ++ ".log"

failureCategory :: String -> String
failureCategory diagnostic =
    if any
        (`isInfixOf` diagnostic)
        [ "cannot satisfy -package"
        , "Could not find module"
        , "Could not load module"
        ]
        then "setup error: target modules are unavailable in GHC's environment"
        else "generated-code type error"
