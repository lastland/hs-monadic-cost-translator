-- | Keep handwritten runner inputs in checked-in fixtures, not source strings.
-- This is a bounded coding-policy check, not IO analysis or a security sandbox:
-- it checks the path-taking names below (including qualified references), without
-- resolving imports or types. Shadowed writer names are checked conservatively.
-- Opening APIs count as writers even in ReadMode. Stream output is not checked.
-- Only saturated ordinary applications and literal log leaves are supported;
-- aliases, partial applications and other writer call shapes fail closed.
module FixturePolicy (checkFixturePolicy) where

import Control.Monad (unless)
import Data.Data (Data, cast, gmapQ)
import Data.List (isInfixOf)
import GHC (DynFlags, GhcPs, HsExpr(..), HsLit(..), LHsExpr, getLocA, getSessionDynFlags, runGhc, unLoc)
import GHC.Data.FastString (mkFastString, unpackFS)
import GHC.Data.Bag (bagToList)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Ppr (showSDoc)
import GHC.Paths (libdir)
import qualified GHC.Parser as Parser
import GHC.Parser.Lexer (ParseResult(POk, PFailed), getPsErrorMessages, initParserState, unP)
import GHC.Types.Error (errMsgSpan, getMessages, isEmptyMessages)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated(..), mkRealSrcLoc)
import GHC.Utils.Outputable (ppr)
import Paths_cost_translate (getDataFileName)
import System.FilePath (takeExtension)

checkFixturePolicy :: IO ()
checkFixturePolicy = do
    flags <- runGhc (Just libdir) getSessionDynFlags
    expectPolicy flags "test/cases/fixture-policy-source-write/Example.hs"
        [["Example.hs:5:", "writeFile", "destination"]]
    expectPolicy flags "test/cases/fixture-policy-log-write/Example.hs" []
    expectPolicy flags "test/cases/fixture-policy-alias/Example.hs"
        [["Example.hs:5:", "writeFile", "saturated"]]
    expectPolicy flags "test/cases/fixture-policy-multiline-log/Example.hs" []
    expectPolicy flags "test/cases/fixture-policy-append/Example.hs"
        [["Example.hs:5:", "appendFile", "destination"]]
    expectPolicy flags "test/cases/fixture-policy-open/Example.hs"
        [["Example.hs:6:", "openFile", "destination"]
        ,["Example.hs:7:", "withFile", "destination"]
        ,["Example.hs:8:", "openBinaryFile", "destination"]
        ,["Example.hs:9:", "withBinaryFile", "destination"]]
    expectPolicy flags "test/cases/fixture-policy-probe-name/OutputAstProbe.hs"
        [["OutputAstProbe.hs:3:", "writeFile", "destination"]]
    expectPolicy flags "test/OutputAstProbe.hs" []
    expectPolicy flags "test/cases/fixture-policy-dynamic-path/Example.hs"
        [["Example.hs:3:", "writeFile", "destination"]]
    expectPolicy flags "test/cases/fixture-policy-partial/Example.hs"
        [["Example.hs:5:", "writeFile", "saturated"]
        ,["Example.hs:6:", "appendFile", "saturated"]
        ,["Example.hs:7:", "openFile", "saturated"]
        ,["Example.hs:8:", "withFile", "saturated"]]
    expectPolicy flags "test/cases/fixture-policy-qualified-source/Example.hs"
        [["Example.hs:(7,", "writeFile", "destination"]]
    expectPolicy flags "test/cases/fixture-policy-harmless/Example.hs" []
    expectPolicy flags "test/cases/fixture-policy-open-log/Example.hs" []
    expectPolicy flags "test/cases/fixture-policy-nested-writer/Example.hs"
        [["Example.hs:5:", "writeFile", "destination"]]
    expectPolicy flags "test/cases/fixture-policy-parse-error/Example.hs"
        [["Example.hs:", "fixture policy", "cannot parse"]]
    problems <- checkFile flags "test/Main.hs"
    unless (null problems) (fail (unlines problems))
    putStrLn "PASS: fixture-policy / parsed runner writes, static regressions, explicit AST-probe exemption"

expectPolicy :: DynFlags -> FilePath -> [[String]] -> IO ()
expectPolicy flags file expected = do
    problems <- checkFile flags file
    let matches = length problems == length expected &&
            and (zipWith (\fragments problem -> all (`isInfixOf` problem) fragments) expected problems)
    unless matches $
        fail (file ++ ": expected " ++ show expected ++ ", got " ++ show problems)

checkFile :: DynFlags -> FilePath -> IO [String]
checkFile flags file = do
    path <- getDataFileName file
    source <- readFile path
    pure (checkSource flags file source)

checkSource :: DynFlags -> FilePath -> String -> [String]
checkSource flags file source = case unP Parser.parseModule state of
    PFailed failed -> parseProblem failed
    POk parsedState _ | not (isEmptyMessages (getPsErrorMessages parsedState)) -> parseProblem parsedState
    -- This one probe tests AST emission itself; it must write generated Haskell.
    -- A matching basename elsewhere is deliberately not an exemption.
    POk _ _ | file == "test/OutputAstProbe.hs" -> []
    POk _ parsed -> checkExpressions flags parsed
  where
    state = initParserState (initParserOpts flags) (stringToStringBuffer source)
        (mkRealSrcLoc (mkFastString file) 1 1)
    parseProblem failed =
        let location = case bagToList (getMessages (getPsErrorMessages failed)) of
                problem:_ -> showSDoc flags (ppr (errMsgSpan problem))
                [] -> file ++ ":1"
        in [location ++ ": fixture policy: cannot parse Haskell input"]

checkExpressions :: Data a => DynFlags -> a -> [String]
checkExpressions flags value = case cast value :: Maybe (LHsExpr GhcPs) of
    Just expression -> case spine expression of
        (function, arguments) | Just writer <- variable function,
            Just arity <- lookup writer writerArities ->
            let at reason = showSDoc flags (ppr (getLocA expression)) ++
                    ": fixture policy: " ++ writer ++ " " ++ reason ++ "; copy source modules and drivers from test/cases"
                problems = case arguments of
                    destination:_ | length arguments == arity ->
                        [at "destination must be a literal .log path or root </> a literal .log filename"
                        | not (logPath destination)]
                    _ -> [at "must be a saturated direct call; writer aliases and partial applications are unsupported"]
            in problems ++ concatMap (checkExpressions flags) arguments
        _ -> concat (gmapQ (checkExpressions flags) (unLoc expression))
    Nothing -> concat (gmapQ (checkExpressions flags) value)

writerArities :: [(String, Int)]
writerArities =
    [("writeFile", 2), ("appendFile", 2), ("openFile", 2), ("withFile", 3)
    ,("openBinaryFile", 2), ("withBinaryFile", 3)]

spine :: LHsExpr GhcPs -> (LHsExpr GhcPs, [LHsExpr GhcPs])
spine = go []
  where
    go arguments (L _ (HsApp _ function argument)) = go (argument:arguments) function
    go arguments (L _ (HsPar _ expression)) = go arguments expression
    go arguments expression = (expression, arguments)

variable :: LHsExpr GhcPs -> Maybe String
variable (L _ (HsVar _ name)) = Just (occNameString (rdrNameOcc (unLoc name)))
variable _ = Nothing

logPath :: LHsExpr GhcPs -> Bool
logPath (L _ (HsPar _ expression)) = logPath expression
logPath (L _ (OpApp _ _ operator leaf)) | variable operator == Just "</>" = logLiteral leaf
logPath expression = logLiteral expression

logLiteral :: LHsExpr GhcPs -> Bool
logLiteral (L _ (HsPar _ expression)) = logLiteral expression
logLiteral (L _ (HsLit _ (HsString _ path))) = takeExtension (unpackFS path) == ".log"
logLiteral _ = False
