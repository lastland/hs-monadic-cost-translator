module GeneratedFunctions (checkTopLevelFunctions, checkAcceptedFunctions, checkGeneratedFunctionSpecimens, parseHaskellModule) where

import Control.Exception (IOException, displayException, try)
import Control.Monad (unless)
import Cost.Translate (generatedName)
import Cost.Publication (Publication, publicationSyntax)
import Data.List (isInfixOf)
import GHC
    ( GhcPs, HsModule(..), HsDecl(..), HsBindLR(..), MatchGroup(..), Match(..), Pat(..)
    , getLocA, getSessionDynFlags, runGhc, unLoc
    )
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Session (parseDynamicFilePragma)
import GHC.Paths (libdir)
import qualified GHC.Parser as Parser
import GHC.Parser.Header (getOptions)
import GHC.Parser.Lexer (ParseResult(..), getPsErrorMessages, initParserState, unP)
import GHC.Types.Error (isEmptyMessages)
import GHC.Types.Name (mkInternalName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated(..), mkRealSrcLoc)
import GHC.Types.Unique (mkUniqueGrimily)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import Paths_cost_translate (getDataFileName)
import System.FilePath ((</>))

-- Only declarations directly under the module count. In particular, selectors,
-- signatures and implementations inside an initializer are not exported values.
checkTopLevelFunctions :: FilePath -> FilePath -> IO ()
checkTopLevelFunctions source output = do
    original <- parseHaskellModule source
    checkFunctionSyntax original output

checkAcceptedFunctions :: Publication -> FilePath -> IO ()
checkAcceptedFunctions publication = checkFunctionSyntax (publicationSyntax publication)

checkFunctionSyntax :: HsModule GhcPs -> FilePath -> IO ()
checkFunctionSyntax original output = do
    generated <- parseHaskellModule output
    let bindings = concatMap topLevelNames (hsmodDecls generated)
        missing =
            [ showSDocUnsafe (ppr location) ++ ": top-level source function " ++
              occNameString occurrence ++ " requires module-level generated binding " ++
              expected ++ " in " ++ output
            | L _ (ValD _ (FunBind _ name matches)) <- hsmodDecls original
            , not (all (null . m_pats . unLoc) (unLoc (mg_alts matches)))
            , let occurrence = rdrNameOcc (unLoc name)
                  location = getLocA name
                  expected = generatedName "M" (mkInternalName (mkUniqueGrimily 0) occurrence location)
            , expected `notElem` bindings
            ]
    unless (null missing) (fail (unlines missing))
  where
    topLevelNames (L _ (ValD _ (FunBind _ name _))) = [occNameString (rdrNameOcc (unLoc name))]
    topLevelNames (L _ (ValD _ (PatBind _ pattern' _ _))) = patternName (unLoc pattern')
    topLevelNames _ = []

    patternName (VarPat _ name) = [occNameString (rdrNameOcc (unLoc name))]
    patternName (ParPat _ nested) = patternName (unLoc nested)
    patternName (SigPat _ nested _) = patternName (unLoc nested)
    patternName (BangPat _ nested) = patternName (unLoc nested)
    patternName (LazyPat _ nested) = patternName (unLoc nested)
    patternName _ = []

-- Read each file's pragmas without loading its imports. This parser is shared by
-- the global guard and the existing, more specific generated-output checks.
parseHaskellModule :: FilePath -> IO (HsModule GhcPs)
parseHaskellModule file = do
    source <- readFile file
    flags <- runGhc (Just libdir) getSessionDynFlags
    let buffer = stringToStringBuffer source
        (headerErrors, options) = getOptions (initParserOpts flags) buffer file
    unless (isEmptyMessages headerErrors) (fail (file ++ ": cannot parse Haskell pragmas"))
    (fileFlags, leftovers, _) <- parseDynamicFilePragma flags options
    unless (null leftovers) (fail (file ++ ": unrecognized Haskell file options"))
    case unP Parser.parseModule (initParserState (initParserOpts fileFlags)
                buffer (mkRealSrcLoc (mkFastString file) 1 1)) of
        POk state parsed | isEmptyMessages (getPsErrorMessages state) -> pure (unLoc parsed)
        _ -> fail (file ++ ": cannot parse Haskell")

-- Every specimen goes through the same path-taking checker as real artifacts.
-- These are parser/contract specimens, not generated/reference execution cases.
checkGeneratedFunctionSpecimens :: IO ()
checkGeneratedFunctionSpecimens = do
    accepts "Source.hs" "ModuleLevel.hs"
    accepts "Single.hs" "Value.hs"
    accepts "Single.hs" "PatternVariable.hs"
    mapM_ (rejects "Single.hs" "f" "fM")
        ["NestedLet.hs", "NestedWhere.hs", "RenamedBundle.hs", "FieldsOnly.hs", "SignatureOnly.hs", "Missing.hs"]
    rejects "Unexported.hs" "hidden" "hiddenM" "Value.hs"
    putStrLn "PASS: generated-functions / module-level values, equations, recursion, operators, pragmas, local helpers / nested, selector, signature and missing bindings rejected"
  where
    specimen name = getDataFileName ("test" </> "generated-functions" </> name)
    accepts sourceName outputName = do
        source <- specimen sourceName
        output <- specimen outputName
        checkTopLevelFunctions source output
    rejects sourceName function expected outputName = do
        source <- specimen sourceName
        output <- specimen outputName
        result <- try (checkTopLevelFunctions source output) :: IO (Either IOException ())
        case result of
            Left problem | all (`isInfixOf` displayException problem)
                    [source ++ ":3:", "top-level source function " ++ function,
                     "module-level generated binding " ++ expected, output] -> pure ()
            _ -> fail (output ++ ": expected missing top-level " ++ expected ++ ", got " ++ show result)
