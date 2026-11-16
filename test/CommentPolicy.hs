module CommentPolicy (main) where

import Data.Char (isSpace, toLower)
import Data.List (stripPrefix)
import GHC (DynFlags, getSessionDynFlags, runGhc)
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Ppr (showSDoc)
import GHC.Driver.Session (parseDynamicFilePragma)
import GHC.Parser.Header (getOptions)
import GHC.Parser.Lexer (ParseResult(..), PState, Token(..), getPsErrorMessages, lexTokenStream)
import GHC.Types.Error (NoDiagnosticOpts(..), isEmptyMessages)
import GHC.Types.SrcLoc (GenLocated(..), Located, mkRealSrcLoc)
import GHC.Utils.Outputable (ppr)
import GHC.Utils.Error (pprMessages)
import System.Environment (getArgs)
import System.Exit (die)

checkComments :: DynFlags -> FilePath -> String -> IO ()
checkComments flags path source = do
    tokens <- rawTokens flags path source
    case [location | L location token <- tokens, isComment token] of
        [] -> pure ()
        location : _ -> die (showSDoc flags (ppr location) ++ ": explanatory comment")

rawTokens :: DynFlags -> FilePath -> String -> IO [Located Token]
rawTokens flags path source = do
    let buffer = stringToStringBuffer (unlines (map maskDirective (lines source)))
        (headerErrors, options) = getOptions (initParserOpts flags) buffer path
    if isEmptyMessages headerErrors then pure () else die (path ++ ": cannot lex Haskell pragmas")
    (fileFlags, leftovers, _) <- parseDynamicFilePragma flags options
    if null leftovers then pure () else die (path ++ ": unrecognized Haskell file options")
    case lexTokenStream (initParserOpts fileFlags) buffer (mkRealSrcLoc (mkFastString path) 1 1) of
        POk state tokens | isEmptyMessages (getPsErrorMessages state) -> pure tokens
        POk state _ -> lexFailure flags path state
        PFailed state -> lexFailure flags path state

lexFailure :: DynFlags -> FilePath -> PState -> IO a
lexFailure flags path state = die (path ++ ": cannot lex Haskell\n" ++
    showSDoc flags (pprMessages NoDiagnosticOpts (getPsErrorMessages state)))

isComment :: Token -> Bool
isComment token = case token of
    ITlineComment {} -> True
    ITblockComment text _ -> not (compilerPragma text)
    ITdocComment {} -> True
    ITdocOptions {} -> True
    _ -> False

compilerPragma :: String -> Bool
compilerPragma text = case fmap words (stripPrefix "{-#" text) of
    Just (name : _) -> map toLower name `elem` ["language", "options", "options_ghc", "include"]
    _ -> False

maskDirective :: String -> String
maskDirective line
    | cppDirective line = map (const ' ') line
    | otherwise = line

cppDirective :: String -> Bool
cppDirective line = case dropWhile isSpace line of
    '#' : rest -> case words rest of
        name : _ -> name `elem`
            ["if", "ifdef", "ifndef", "else", "elif", "endif", "define", "undef",
             "include", "line", "error", "warning", "pragma"]
        [] -> False
    _ -> False

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        directory : "--check" : files@(_ : _) -> do
            flags <- runGhc (Just directory) getSessionDynFlags
            mapM_ (checkFile flags) files
        _ -> die "Usage: comment-policy LIBDIR --check FILE..."
  where
    checkFile flags path = do
        source <- readFile path
        checkComments flags path source
