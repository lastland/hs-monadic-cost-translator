-- | Fatal setup failures are distinct from ordinary rejected reference cases.
-- Only this adapter interprets unavoidable diagnostics from external boundaries.
module ReferenceFailure
    ( FatalSetupFailure(..), fatalSetupFailure
    , ExternalDiagnostic(..), externalFailure
    ) where

import Control.Exception (Exception, throwIO)
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf)

newtype FatalSetupFailure = FatalSetupFailure String

instance Show FatalSetupFailure where
    show (FatalSetupFailure message) = "reference setup error: " ++ message

instance Exception FatalSetupFailure

fatalSetupFailure :: String -> IO a
fatalSetupFailure = throwIO . FatalSetupFailure

data ExternalDiagnostic = TranslationDiagnostic | CompilationDiagnostic | ComparisonDiagnostic

externalFailure :: ExternalDiagnostic -> String -> String -> IO a
externalFailure origin context diagnostic =
    if isSetup origin diagnostic then fatalSetupFailure message else fail message
  where
    message = context ++ "\n" ++ diagnostic

isSetup :: ExternalDiagnostic -> String -> Bool
isSetup TranslationDiagnostic diagnostic =
    "setup error:" `isPrefixOf` diagnostic ||
    isSetup CompilationDiagnostic diagnostic ||
    ("source error:" `isPrefixOf` diagnostic &&
     any (`isInfixOf` diagnostic)
        ["unknown symbol", "undefined symbol", "Symbol not found", "dlopen", "panic!"])
isSetup CompilationDiagnostic diagnostic =
    any (`isInfixOf` diagnostic)
        ["cannot satisfy -package", "can't find a package database", "ghc: panic!"]
isSetup ComparisonDiagnostic diagnostic = any protocolFailure (lines diagnostic)
  where
    protocolFailure line =
        let body = dropWhile isSpace (drop 1 (dropWhile (/= ':') line))
        in "user error (observer timed out or returned an invalid response" `isPrefixOf` body ||
           (("fd:" `isPrefixOf` body || "hPut" `isPrefixOf` body) &&
            any (`isInfixOf` body) ["hGetLine: end of file", "resource vanished (Broken pipe)"])
