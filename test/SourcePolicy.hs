module SourcePolicy (main) where

import Control.Exception (IOException, catch, evaluate)
import Control.Monad (unless, when)
import qualified Data.ByteString as ByteString
import Data.Char (isDigit, isSpace, ord, toLower)
import Data.Data (Data, cast, gmapQ)
import Data.Foldable (for_)
import qualified Data.IntMap.Strict as IntMap
import Data.List (intercalate, isPrefixOf, sort, sortOn, stripPrefix)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC
    ( DynFlags, GhcPs, HsBind, HsBindLR(..), HsCmd(..), HsDecl(..), HsExpr(..)
    , HsIPName(..), HsModule(..), IPBind(..), LHsBind, LHsCmd, LHsDecl, LHsExpr, LIPBind
    , getLocA, getSessionDynFlags, runGhc, unLoc
    )
import GHC.Data.FastString (mkFastString, unpackFS)
import GHC.Data.Bag (bagToList)
import GHC.Data.StringBuffer (StringBuffer, stringToStringBuffer)
import qualified GHC.Data.Strict as Strict
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Session (GeneralFlag(..), gopt, parseDynamicFilePragma, xopt)
import qualified GHC.LanguageExtensions.Type as Extension
import qualified GHC.Parser as Parser
import GHC.Parser.Header (getOptions)
import GHC.Parser.Lexer
    ( ExtBits(..), PState(loc), ParserOpts(..), ParseResult(..), Token(..)
    , getPsErrorMessages, initParserState, lexer, unP, xset, xunset
    )
import GHC.Types.Error (Messages, errMsgSpan, getMessages, isEmptyMessages)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SourceError (handleSourceError, srcErrorMessages)
import GHC.Types.SrcLoc
    ( BufPos(..), BufSpan(..), GenLocated(..), Located, SrcSpan(..), getBufSpan
    , PsLoc(psBufPos), RealSrcLoc, mkRealSrcLoc, mkRealSrcSpan, noLoc
    , srcSpanStartLine, srcSpanStartCol, srcSpanEndLine, srcSpanEndCol
    )
import Numeric (showHex)
import GHC.Utils.Panic (handleGhcException)
import System.Environment (getArgs)
import System.Directory (pathIsSymbolicLink)
import System.Exit (die)
import System.FilePath ((</>), isAbsolute, normalise, splitDirectories, takeExtension)
import System.IO (hSetEncoding, stdout, utf8)
import System.Posix.Files (getSymbolicLinkStatus, isRegularFile)

data Definition = Definition SrcSpan String

type Positions = IntMap.IntMap (Int, Int)

sourceFlags :: DynFlags -> FilePath -> String -> IO DynFlags
sourceFlags defaults path text = do
    (base, _, _) <- parseDynamicFilePragma defaults [noLoc "-XGHC2021"]
    let source = stringToStringBuffer text
    options <- handleSourceError (diagnosticFailure path text "cannot parse Haskell pragmas" Nothing . srcErrorMessages) $ do
        let (errors, found) = getOptions (initParserOpts base) source path
        unless (isEmptyMessages errors) $ do
            let (tokens, _) = rawTokens (initParserOpts base) source (mkRealSrcLoc (mkFastString path) 1 1)
            rejectUnsupportedTokens path text tokens
            diagnosticFailure path text "cannot parse Haskell pragmas" Nothing errors
        _ <- evaluate (sum (map (length . unLoc) found))
        pure found
    (flags, leftovers, _) <- handleGhcException (\_ -> die (path ++ ": cannot apply Haskell file options")) $
        handleSourceError (\_ -> die (path ++ ": cannot apply Haskell file options")) $
            parseDynamicFilePragma base options
    unless (null leftovers) (die (path ++ ": unrecognized Haskell file options"))
    when (xopt Extension.Cpp flags) (die (path ++ ": unsupported preprocessing (CPP)"))
    when (gopt Opt_Pp flags) (die (path ++ ": unsupported preprocessing (-F)"))
    case [prefix | L _ option <- options, prefix <- ["-pgmF", "-pgmP", "-optF", "-optP", "-D", "-U", "-I", "-include", "-#include"]
         , prefix `isPrefixOf` option] of
        prefix : _ -> die (path ++ ": unsupported preprocessing (" ++ prefix ++ ")")
        [] -> pure ()
    pure flags

-- This is GHC's raw-token mode, stepped one token at a time so a rejected
-- preprocessor directive cannot erase the already examined lexical prefix.
rawTokens :: ParserOpts -> StringBuffer -> RealSrcLoc -> ([Located Token], Maybe PState)
rawTokens options source start = go [] (initParserState rawOptions source start)
  where
    rawOptions = options {pExtsBitmap = xunset UsePosPragsBit (xset RawTokenStreamBit (pExtsBitmap options))}
    go collected state = case unP (lexer False pure) state of
        POk next (L _ ITeof) -> (reverse collected, if isEmptyMessages (getPsErrorMessages next) then Nothing else Just next)
        POk next token -> go (token : collected) next
        PFailed failure -> (reverse collected, Just failure)

parseSource :: DynFlags -> FilePath -> String -> IO (HsModule GhcPs, [Located Token])
parseSource flags path text = do
    let options = initParserOpts flags
        source = stringToStringBuffer text
        start = mkRealSrcLoc (mkFastString path) 1 1
    let (tokens, failure) = rawTokens options source start
    rejectUnsupportedTokens path text tokens
    for_ failure (stateFailure path text "cannot lex Haskell")
    parsed <- case unP Parser.parseModule (initParserState options source start) of
        POk state result | isEmptyMessages (getPsErrorMessages state) -> pure (unLoc result)
        POk state _ -> stateFailure path text "cannot parse Haskell" state
        PFailed state -> stateFailure path text "cannot parse Haskell" state
    pure (parsed, tokens)

stateFailure :: FilePath -> String -> String -> PState -> IO a
stateFailure path text description state = diagnosticFailure path text description
    (Just (bufPos (psBufPos (loc state)))) (getPsErrorMessages state)

diagnosticFailure :: FilePath -> String -> String -> Maybe Int -> Messages e -> IO a
diagnosticFailure path text description fallback messages = die (path ++ suffix ++ ": " ++ description)
  where
    offsets = sort [start | message <- bagToList (getMessages messages)
                   , Just (start, _) <- [spanOffsets (errMsgSpan message)]]
    suffix = case offsets ++ maybe [] pure fallback of
        start : _ -> let (line, column) = offsetPosition text start
                     in ":" ++ show line ++ ":" ++ show column
        [] -> ""

spanOffsets :: SrcSpan -> Maybe (Int, Int)
spanOffsets location = case getBufSpan location of
    Strict.Just (BufSpan (BufPos start) (BufPos end)) -> Just (start, end)
    Strict.Nothing -> Nothing

offsetPosition :: String -> Int -> (Int, Int)
offsetPosition text offset = foldl' advancePosition (1, 1) (take offset text)

advancePosition :: (Int, Int) -> Char -> (Int, Int)
advancePosition (line, _) '\n' = (line + 1, 1)
advancePosition (line, column) '\t' = (line, ((column - 1) `div` 8 + 1) * 8 + 1)
advancePosition (line, column) _ = (line, column + 1)

-- BufPos counts Unicode code points and cannot be changed by LINE directives,
-- including the #line text that GHC interprets inside ordinary block comments.
physicalLocation :: FilePath -> Positions -> SrcSpan -> IO SrcSpan
physicalLocation path positions location = case spanOffsets location of
    Just (start, end)
        | Just (startLine, startColumn) <- IntMap.lookup start positions
        , Just (endLine, endColumn) <- IntMap.lookup end positions ->
            pure (RealSrcSpan (mkRealSrcSpan (point startLine startColumn) (point endLine endColumn))
                  (getBufSpan location))
    _ -> die (path ++ ": unsupported source span without physical offsets")
  where
    point = mkRealSrcLoc (mkFastString path)

physicalDefinition :: FilePath -> Positions -> Definition -> IO Definition
physicalDefinition path positions (Definition location name) = do
    physical <- physicalLocation path positions location
    pure (Definition physical name)

rejectUnsupportedTokens :: FilePath -> String -> [Located Token] -> IO ()
rejectUnsupportedTokens path text tokens = case sort occurrences of
    [] -> pure ()
    (offset, message) : _ -> do
        let (line, column) = offsetPosition text offset
        die (path ++ ":" ++ show line ++ ":" ++ show column ++ ": " ++ message)
  where
    occurrences =
        [(start, message) | L location token <- tokens, Just message <- [unsupported token]
        , Just (start, _) <- [spanOffsets location]] ++
        [(offset + length indentation, message)
        | (offset, line) <- zip (scanl (\n line -> n + length line + 1) 0 (lines text)) (lines text)
        , let (indentation, content) = span isSpace line
        , not (any (protects (offset + length indentation)) tokens)
        , Just message <- [hashDirective content]]

    unsupported ITline_prag{} = Just "unsupported source-location directive"
    unsupported ITcolumn_prag{} = Just "unsupported source-location directive"
    unsupported ITprimstring{} = Just "unsupported primitive byte-string literal"
    unsupported (ITblockComment comment _)
        | Just rest <- stripPrefix "{-#" comment
        , "include" : _ <- map (map toLower) (words rest) = Just "unsupported preprocessing directive"
    unsupported _ = Nothing

    hashDirective ('#' : rest) = case dropWhile isSpace rest of
        c : _ | isDigit c -> Just "unsupported source-location directive"
        '!' : _ -> Just "unsupported preprocessing directive"
        remaining -> case words remaining of
            "line" : _ -> Just "unsupported source-location directive"
            name : _ | name `elem` ["if", "ifdef", "ifndef", "else", "elif", "elifdef", "elifndef", "endif", "define", "undef", "include", "error", "warning", "pragma"] ->
                Just "unsupported preprocessing directive"
            [] -> Just "unsupported preprocessing directive"
            _ -> Nothing
    hashDirective _ = Nothing

    protects offset (L location token) = protected token && case spanOffsets location of
        Just (start, end) -> start <= offset && offset < end
        Nothing -> False

    protected ITblockComment{} = True
    protected ITlineComment{} = True
    protected ITdocComment{} = True
    protected ITdocOptions{} = True
    protected ITstring{} = True
    protected ITchar{} = True
    protected ITquasiQuote{} = True
    protected ITqQuasiQuote{} = True
    protected _ = False

tokenLines :: Located Token -> [Int]
tokenLines (L _ ITlineComment{}) = []
tokenLines (L location (ITblockComment text _))
    | "{-#" `isPrefixOf` text = spanLines location
    | otherwise = []
tokenLines (L _ ITdocComment{}) = []
tokenLines (L _ ITdocOptions{}) = []
tokenLines (L _ ITvocurly) = []
tokenLines (L _ ITvccurly) = []
tokenLines (L location _) = spanLines location

spanLines :: SrcSpan -> [Int]
spanLines (RealSrcSpan location _) =
    [srcSpanStartLine location .. srcSpanEndLine location - if srcSpanEndCol location == 1 then 1 else 0]
spanLines _ = []

topDefinitions :: HsModule GhcPs -> [Definition]
topDefinitions = concatMap declarationDefinition . hsmodDecls

declarationDefinition :: LHsDecl GhcPs -> [Definition]
declarationDefinition declaration@(L _ (ValD _ binding)) = bindDefinition (getLocA declaration) binding
declarationDefinition _ = []

bindDefinition :: SrcSpan -> HsBind GhcPs -> [Definition]
bindDefinition location (FunBind _ name _) =
    [Definition location (occNameString (rdrNameOcc (unLoc name)))]
bindDefinition location PatBind{} = [Definition location "<pattern>"]
bindDefinition _ _ = []

-- Query located nodes, not names: shadowed bindings have independent spans,
-- while all equations of a FunBind belong to the same located declaration.
allDefinitions :: Data a => a -> [Definition]
allDefinitions node = here ++ concat (gmapQ allDefinitions node)
  where
    here = maybe [] declarationDefinition (cast node) ++
        maybe [] locatedBinding (cast node) ++
        maybe [] expressionLambda (cast node) ++
        maybe [] commandLambda (cast node) ++
        maybe [] implicitBinding (cast node)

locatedBinding :: LHsBind GhcPs -> [Definition]
locatedBinding binding = bindDefinition (getLocA binding) (unLoc binding)

expressionLambda :: LHsExpr GhcPs -> [Definition]
expressionLambda expression@(L _ HsLam{}) = [Definition (getLocA expression) "<lambda>"]
expressionLambda _ = []

commandLambda :: LHsCmd GhcPs -> [Definition]
commandLambda command@(L _ HsCmdLam{}) = [Definition (getLocA command) "<lambda>"]
commandLambda _ = []

implicitBinding :: LIPBind GhcPs -> [Definition]
implicitBinding binding@(L _ (IPBind _ name _)) =
    let HsIPName identifier = unLoc name
    in [Definition (getLocA binding) ('?' : unpackFS identifier)]

-- A pattern synonym is not an ordinary pattern binding. Until its counting
-- policy is agreed, reject it rather than return a plausible but incomplete row.
unsupportedBindings :: Data a => a -> [SrcSpan]
unsupportedBindings node = here ++ concat (gmapQ unsupportedBindings node)
  where
    here = case cast node :: Maybe (LHsDecl GhcPs) of
        Just declaration@(L _ (ValD _ PatSynBind{})) -> [getLocA declaration]
        _ -> case cast node :: Maybe (LHsBind GhcPs) of
            Just binding@(L _ PatSynBind{}) -> [getLocA binding]
            _ -> []

definitionOrder :: Definition -> (Int, Int, String)
definitionOrder (Definition (RealSrcSpan location _) name) =
    (srcSpanStartLine location, srcSpanStartCol location, name)
definitionOrder (Definition _ name) = (0, 0, name)

position :: SrcSpan -> [(String, String)]
position (RealSrcSpan location _) =
    [("line", show (srcSpanStartLine location)), ("column", show (srcSpanStartCol location))]
position _ = []

definitionJson :: Set.Set Int -> Bool -> Definition -> String
definitionJson code sized (Definition location name) = object
    (position location ++ [("name", jsonString name)] ++
     [("lines", show (length (filter (`Set.member` code) (spanLines location)))) | sized])

stringLiterals :: [Located Token] -> [String]
stringLiterals tokens =
    [object (position location ++ [("value", jsonString (unpackFS value))])
    | L location (ITstring _ value) <- tokens]

checkFile :: DynFlags -> FilePath -> IO String
checkFile defaults path = do
    unless (takeExtension path == ".hs") (die (path ++ ": unsupported source input (expected .hs)"))
    source <- readSource path
    flags <- sourceFlags defaults path source
    (parsed, raw) <- parseSource flags path source
    let positions = IntMap.fromDistinctAscList (zip [0 ..] (scanl advancePosition (1, 1) source))
    case sortOn spanOffsets (unsupportedBindings parsed) of
        [] -> pure ()
        location : _ -> case spanOffsets location of
            Just (offset, _) -> do
                let (line, column) = offsetPosition source offset
                die (path ++ ":" ++ show line ++ ":" ++ show column ++ ": unsupported pattern-synonym binding")
            Nothing -> die (path ++ ": unsupported pattern-synonym binding")
    tokens <- mapM (\(L location token) -> do
        physical <- physicalLocation path positions location
        pure (L physical token)) raw
    topLevel <- mapM (physicalDefinition path positions) (topDefinitions parsed)
    definitions <- sortOn definitionOrder <$> mapM (physicalDefinition path positions) (allDefinitions parsed)
    let code = Set.fromList (concatMap tokenLines tokens)
    pure (object
        [("path", jsonString path), ("code_lines", array (map show (Set.toAscList code)))
        , ("top_level", array (map (definitionJson code False) topLevel))
        , ("definitions", array (map (definitionJson code True) definitions))
        , ("strings", array (stringLiterals tokens))])

readSource :: FilePath -> IO String
readSource path = catch readUtf8 readFailure
  where
    readUtf8 = do
        mapM_ rejectLink (scanl1 (</>) (splitDirectories path))
        status <- getSymbolicLinkStatus path
        unless (isRegularFile status) (die (path ++ ": unsupported non-regular source file"))
        bytes <- ByteString.readFile path
        case Text.decodeUtf8' bytes of
            Left _ -> die (path ++ ": invalid UTF-8")
            Right source -> pure (Text.unpack source)
    rejectLink component = do
        symbolic <- pathIsSymbolicLink component
        when symbolic (die (path ++ ": unsupported symbolic link"))
    readFailure :: IOException -> IO String
    readFailure _ = die (path ++ ": cannot read source file")

object :: [(String, String)] -> String
object fields = "{" ++ intercalate "," [jsonString key ++ ":" ++ value | (key, value) <- fields] ++ "}"

array :: [String] -> String
array values = "[" ++ intercalate "," values ++ "]"

jsonString :: String -> String
jsonString value = '"' : concatMap escape value ++ "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape character
        | ord character < 32 || ord character >= 0xd800 && ord character <= 0xdfff ||
          character `elem` "\x85\x2028\x2029" =
            let hexadecimal = showHex (ord character) ""
            in "\\u" ++ replicate (4 - length hexadecimal) '0' ++ hexadecimal
        | otherwise = [character]

initialFlags :: FilePath -> IO DynFlags
initialFlags directory = handleGhcException (const unavailable) $
    catch (runGhc (Just directory) getSessionDynFlags) readFailure
  where
    unavailable = die "source-policy-engine: cannot initialize GHC"
    readFailure :: IOException -> IO DynFlags
    readFailure _ = unavailable

main :: IO ()
main = do
    hSetEncoding stdout utf8
    arguments <- getArgs
    case arguments of
        directory : files@(_ : _) -> do
            when (any (\path -> isAbsolute path || ".." `elem` splitDirectories path) files)
                (die "source-policy-engine: expected repository-relative paths without '..'")
            flags <- initialFlags directory
            results <- mapM (checkFile flags) (Set.toAscList (Set.fromList (map normalise files)))
            mapM_ putStrLn results
        _ -> die "Usage: source-policy-engine LIBDIR FILE..."
