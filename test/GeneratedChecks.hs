module GeneratedChecks (checkGeneratedArtifacts, checkScopedRenamingSources) where

import ConditionChecks (checkConditionOutput)
import Control.Monad (unless, when)
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf, sort, tails)
import GHC
    ( GhcPs, HsExpr(..), LHsExpr, MatchGroup(..), Match(..), Pat(..), HsDoFlavour(..), StmtLR(..)
    , moduleNameString, unLoc
    )
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName(..), rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated(..))
import GeneratedSyntax (bareExpression, collectAst, expressionVariable, generatedBindings, terminalExpression)
import PaperScalarChecks (checkPaperScalarOutput)
import Paths_cost_translate (getDataFileName)
import qualified ReferenceRunner
import ReferenceFailure (fatalSetupFailure, ExternalDiagnostic(..), externalFailure)
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeFileName)
import System.Process (readProcessWithExitCode)

-- These checks inspect the artifacts already compared with the handwritten
-- reference. They neither translate again nor run fixture-specific drivers.
checkGeneratedArtifacts :: String -> ReferenceRunner.Artifacts -> IO ()
checkGeneratedArtifacts label artifacts = do
    when ("manual-" `isPrefixOf` label) (checkManualSourceWarnings artifacts)
    case label of
        "generated-warnings" -> checkGeneratedWarnings root
        "generated-warnings-minimal" -> checkMinimalWarnings root
        "ordered-equations-and-conjunctive-guards" -> listInspection "chooseM"
        "manual-constructor-equations" -> mapM_ listInspection ["chooseM", "repeatedM"]
        "ordered-match-columns" -> checkParameterInspection output "chooseM" 2 ["JustT", "NothingT"]
        "direct-primitive-conditions" -> generatedBindings output >>= checkConditionOutput
        "paper-strict-scalars" -> generatedBindings output >>= checkPaperScalarOutput
        "anf-demand" -> checkTerminalDoBlocks output "chooseM"
        "manual-terminal-do" -> mapM_ (checkTerminalDoBlocks output) ["chooseM", "localM"]
        "pattern-scaling" -> do
            generated <- readFile output
            unless (length generated < 40000) $
                fail ("fallback duplication: " ++ show (length generated) ++ " bytes")
        "foldr-arity-four-partial" -> do
            logText <- readFile (output ++ ".log")
            unless ("Example.add4: supplied 2 / arity 4; partial adapter" `isInfixOf` logText &&
                    not ("Example.add4: supplied 1" `isInfixOf` logText) &&
                    not ("foldr: supplied 1" `isInfixOf` logText)) $
                fail "arity log must describe maximal applications, not intermediate AST spines"
        _ -> pure ()
  where
    root = ReferenceRunner.generatedDirectory artifacts
    output = root </> "Generated.hs"
    listInspection target = checkParameterInspection output target 1 ["NilT", "ConsT"]

-- Check maintained example sources separately. Generated-output warning checks
-- retain their existing, explicitly bounded generated-warnings* scope.
checkManualSourceWarnings :: ReferenceRunner.Artifacts -> IO ()
checkManualSourceWarnings artifacts = do
    let root = ReferenceRunner.generatedDirectory artifacts
        source = root </> takeFileName (ReferenceRunner.sourceFixture artifacts)
    (compiled, out, err) <- readProcessWithExitCode "ghc"
        ["-v0", "-fno-code", "-fforce-recomp", "-Wall", "-Werror",
         "-outputdir", root </> "source-objects", source] ""
    let diagnostic = out ++ "\n" ++ err
    writeFile (root </> "source-warnings.log") diagnostic
    ReferenceRunner.checkProcessCrash compiled diagnostic
    unless (compiled == ExitSuccess) $
        externalFailure CompilationDiagnostic "manual source warning check failed" diagnostic
    hlint <- findExecutable "hlint"
    command <- maybe (fatalSetupFailure "manual example lint requires hlint on PATH") pure hlint
    (linted, lintOut, lintErr) <- readProcessWithExitCode command [source] ""
    writeFile (root </> "source-hlint.log") (lintOut ++ lintErr)
    ReferenceRunner.checkProcessCrash linted (lintOut ++ lintErr)
    unless (linted == ExitSuccess) $
        externalFailure CompilationDiagnostic "manual source lint check failed" (lintOut ++ lintErr)

-- Follow the selected parameter's force to its result binder. A case on a
-- field or another argument of the same type is a different inspection.
checkParameterInspection :: FilePath -> String -> Int -> [String] -> IO ()
checkParameterInspection output target position constructors = do
    select <- generatedBindings output
    binding <- select target
    let label = target ++ ", parameter " ++ show position
    parameter <- case drop (position - 1) (m_pats binding) of
        L _ (VarPat _ name) : _ -> pure (unLoc name)
        _ -> fail (label ++ ": expected a named parameter")
    let expressions = collectAst (m_grhss binding) :: [HsExpr GhcPs]
        forces = length (filter ((== Just parameter) . forcedVariable) expressions)
        caseHeads =
            [ [occNameString (rdrNameOcc (unLoc name)) | L _ alt <- unLoc (mg_alts matches),
                   [L _ (ConPat _ name _)] <- [m_pats alt]]
            | HsDo _ (DoExpr Nothing) statements <- expressions
            , L _ (BindStmt _ (L _ (VarPat _ result)) action) : rest <- tails (unLoc statements)
            , forcedVariable (bareExpression action) == Just parameter
            , HsCase _ subject matches <- collectAst rest :: [HsExpr GhcPs]
            , expressionVariable subject == Just (unLoc result)
            ]
    unless (forces == 1 && map sort caseHeads == [sort constructors]) $
        fail (label ++ ": expected one force and one case with constructors " ++ show constructors ++
              "; found " ++ show forces ++ " forces and case constructors " ++ show caseHeads)

forcedVariable :: HsExpr GhcPs -> Maybe RdrName
forcedVariable (HsApp _ function argument)
    | Just (Qual qualifier occurrence) <- expressionVariable function
    , moduleNameString qualifier == "M", occNameString occurrence == "force" = expressionVariable argument
forcedVariable _ = Nothing

checkTerminalDoBlocks :: FilePath -> String -> IO ()
checkTerminalDoBlocks output target = do
    select <- generatedBindings output
    binding <- select target
    let nested = [() | HsDo _ (DoExpr Nothing) statements <- collectAst (m_grhss binding) :: [HsExpr GhcPs],
            terminal : _ <- [reverse (unLoc statements)],
            Just expression <- [terminalExpression terminal],
            terminalOrdinaryDo expression]
    unless (null nested) $
        fail (target ++ ": terminal ordinary do remains nested directly or through parentheses/let; " ++
              "terminal actions must share their enclosing do block")
  where
    terminalOrdinaryDo :: LHsExpr GhcPs -> Bool
    terminalOrdinaryDo expression = case unLoc expression of
        HsDo _ (DoExpr Nothing) _ -> True
        HsPar _ nested -> terminalOrdinaryDo nested
        HsLet _ _ body -> terminalOrdinaryDo body
        _ -> False

checkGeneratedWarnings :: FilePath -> IO ()
checkGeneratedWarnings root = do
    generated <- readFile (root </> "Generated.hs")
    checkGeneratedLayout "Generated.hs" generated
    unless (all (`isInfixOf` generated) ["capturedM x", "add z", "chooseM arg0", "partial'' <-"]) $
        fail "generated local names do not preserve readable source names"
    when (any (`isInfixOf` generated) ["_arg", "argument_", "partial0", "arg0'"]) $
        fail "generated output contains opaque local names"
    let banners = filter ("-- ==== " `isPrefixOf`) (lines generated)
        expectedBanners =
            [ "-- ==== Common: sharing and scalar support ===="
            , "-- ==== Common: foldable support ===="
            , "-- ==== Common: datatype conversion and demand support ===="
            , "-- ==== Module-specific: source datatypes and conversions from Example ===="
            , "-- ==== Module-specific: translated bindings from Example ===="
            ]
    unless (banners == expectedBanners) $
        fail ("wrong generated section banners: " ++ show banners)
    unless ("{-# OPTIONS_GHC -Wno-orphans -Wno-simplifiable-class-constraints #-}" `isInfixOf` generated) $
        fail "generated warning suppressions are missing"
    checkWarningsAndHLint root "warnings.log"

checkMinimalWarnings :: FilePath -> IO ()
checkMinimalWarnings root = do
    minimal <- readFile (root </> "Generated.hs")
    checkGeneratedLayout "Generated.hs" minimal
    when (any (`isInfixOf` minimal) ["Data.Kind", "Data.Proxy", "GHC.TypeLits", "qualified Minimal as Source"]) $
        fail "minimal generated module has unnecessary imports"
    checkWarningsAndHLint root "minimal-warnings.log"

checkWarningsAndHLint :: FilePath -> FilePath -> IO ()
checkWarningsAndHLint root warningLog = do
    let output = root </> "Generated.hs"
    (compiled, out, err) <- readProcessWithExitCode "ghc"
        ["-v0", "-fno-code", "-fforce-recomp", "-Wall", "-Werror",
         "-package", "MemoristANT", "-package", "QuickCheck", "-i", "-i" ++ root, output] ""
    let diagnostic = out ++ "\n" ++ err
    writeFile (root </> warningLog) diagnostic
    ReferenceRunner.checkProcessCrash compiled diagnostic
    unless (compiled == ExitSuccess) $
        externalFailure CompilationDiagnostic "generated warning check failed" diagnostic
    hlint <- findExecutable "hlint"
    case hlint of
        Nothing -> fatalSetupFailure "generated code HLint requires hlint"
        Just command -> do
            (linted, lintOut, lintErr) <- readProcessWithExitCode command [output] ""
            writeFile (root </> "hlint.log") (lintOut ++ lintErr)
            ReferenceRunner.checkProcessCrash linted (lintOut ++ lintErr)
            unless (linted == ExitSuccess) $
                externalFailure CompilationDiagnostic "generated lint check failed" (lintOut ++ lintErr)

checkGeneratedLayout :: FilePath -> String -> IO ()
checkGeneratedLayout name source = do
    unless (hasOneFinalNewline source) $
        fail (name ++ ": expected exactly one final newline")
    when ("\n\n\n" `isInfixOf` source) $
        fail (name ++ ": found a run of three newlines")
    unless (all noTrailingWhitespace sourceLines) $
        fail (name ++ ": found trailing whitespace")
    moduleLine <- exactlyOne "module header" (positions ("module " `isPrefixOf`) sourceLines)
    let plainImports = positions isPlainImport sourceLines
        qualifiedImports = positions ("import qualified " `isPrefixOf`) sourceLines
        declarations = positions isTopLevelDeclaration sourceLines
    case (plainImports, qualifiedImports, reverse plainImports, reverse qualifiedImports) of
        (firstPlain:_, firstQualified:_, lastPlain:_, lastQualified:_) -> do
            unless (firstPlain == moduleLine + 2 && sourceLines !! (moduleLine + 1) == "") $
                fail (name ++ ": expected one blank line after the module header")
            unless (consecutive plainImports && consecutive qualifiedImports &&
                    firstQualified == lastPlain + 2 && sourceLines !! (lastPlain + 1) == "") $
                fail (name ++ ": expected plain and qualified import groups separated by one blank line")
            unless (importLines plainImports == sort (importLines plainImports) &&
                    importLines qualifiedImports == sort (importLines qualifiedImports)) $
                fail (name ++ ": expected alphabetically sorted import groups")
            unless (lastQualified + 1 < length sourceLines && sourceLines !! (lastQualified + 1) == "") $
                fail (name ++ ": expected a blank line after imports")
        _ -> fail (name ++ ": expected plain and qualified import groups")
    when (null declarations) $
        fail (name ++ ": expected at least one top-level declaration")
    case [lineNumber | lineNumber <- declarations,
                       lineNumber == 0 || sourceLines !! (lineNumber - 1) /= "",
                       not (precededBySignature lineNumber)] of
        [] -> pure ()
        lineNumber:_ -> fail (name ++ ": expected one blank line before top-level declaration at line " ++ show (lineNumber + 1))
  where
    sourceLines = lines source
    noTrailingWhitespace line = null line || not (isSpace (last line))
    isPlainImport line = "import " `isPrefixOf` line && not ("import qualified " `isPrefixOf` line)
    isTopLevelDeclaration line@(first:_) = not (isSpace first) &&
        not (any (`isPrefixOf` line) ["{-#", "--", "module ", "import "])
    isTopLevelDeclaration [] = False
    positions predicate = map fst . filter (predicate . snd) . zip [0..]
    importLines = map (sourceLines !!)
    precededBySignature lineNumber = case
        reverse (filter isTopLevelDeclaration (takeWhile (not . null) (reverse (take lineNumber sourceLines)))) of
        [signature] -> "::" `isInfixOf` signature
        _ -> False
    consecutive indexes = and (zipWith (\a b -> b == a + 1) indexes (drop 1 indexes))
    exactlyOne _ [lineNumber] = pure lineNumber
    exactlyOne label found = fail (name ++ ": expected one " ++ label ++ ", found " ++ show (length found))
    hasOneFinalNewline text = case reverse text of
        '\n':previous:_ -> previous /= '\n'
        ['\n'] -> True
        _ -> False

-- Both variants are compared separately against the same handwritten reference;
-- only their source-level relationship needs a separate assertion here.
checkScopedRenamingSources :: IO ()
checkScopedRenamingSources = do
    shadowedPath <- getDataFileName ("test" </> "cases" </> "anf-scoped-renaming" </> "Shadowed.hs")
    renamedPath <- getDataFileName ("test" </> "cases" </> "anf-scoped-renaming" </> "Renamed.hs")
    shadowedSource <- readFile shadowedPath
    renamedSource <- readFile renamedPath
    unless (length (filter (isInfixOf shadowedSignature) (lines shadowedSource)) == 4 &&
            renameInner shadowedSource == renamedSource) $
        fail "scoped-renaming fixtures must differ only in the four inner forall binders"
  where
    shadowedSignature = "inner :: forall a. a -> Int"
    renamedSignature = "inner :: forall b. b -> Int"
    renameInner [] = []
    renameInner input@(c:rest)
        | shadowedSignature `isPrefixOf` input = renamedSignature ++ renameInner (drop (length shadowedSignature) input)
        | otherwise = c : renameInner rest
