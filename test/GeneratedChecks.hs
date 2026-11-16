module GeneratedChecks (checkGeneratedArtifacts, checkScopedRenamingSources) where

import Control.Monad (unless, when)
import Data.Char (isSpace)
import Data.Data (Data, cast, gmapQ)
import Data.List (isInfixOf, isPrefixOf, sort)
import Data.Maybe (maybeToList)
import GHC
    ( GhcPs, HsModule(..), HsDecl(..), HsBindLR(..), HsExpr(..), LHsExpr
    , MatchGroup(..), Match(..), Pat(..), HsDoFlavour(..), StmtLR(..), ExprLStmt
    , moduleNameString, unLoc
    )
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName(..), rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated(..))
import GeneratedFunctions (parseHaskellModule)
import Paths_cost_translate (getDataFileName)
import qualified ReferenceRunner
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
        "ordered-equations-and-conjunctive-guards" -> checkOuterListInspection output
        "manual-constructor-equations" -> checkOuterListInspection output
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

-- Check maintained example sources separately. Generated-output warning checks
-- retain their existing, explicitly bounded generated-warnings* scope.
checkManualSourceWarnings :: ReferenceRunner.Artifacts -> IO ()
checkManualSourceWarnings artifacts = do
    let root = ReferenceRunner.generatedDirectory artifacts
        source = root </> takeFileName (ReferenceRunner.sourceFixture artifacts)
    (compiled, out, err) <- readProcessWithExitCode "ghc"
        ["-v0", "-fno-code", "-fforce-recomp", "-Wall", "-Werror",
         "-outputdir", root </> "source-objects", source] ""
    writeFile (root </> "source-warnings.log") (out ++ err)
    unless (compiled == ExitSuccess) (fail (out ++ err))
    hlint <- findExecutable "hlint"
    command <- maybe (fail "manual example lint requires hlint on PATH") pure hlint
    (linted, lintOut, lintErr) <- readProcessWithExitCode command [source] ""
    writeFile (root </> "source-hlint.log") (lintOut ++ lintErr)
    unless (linted == ExitSuccess) (fail (lintOut ++ lintErr))

checkOuterListInspection :: FilePath -> IO ()
checkOuterListInspection output = do
    binding <- parseGeneratedBinding output "chooseM"
    parameter <- case m_pats binding of
        [L _ (VarPat _ name)] -> pure (unLoc name)
        _ -> fail "chooseM: expected one outer-list parameter"
    let expressions = collectAst (m_grhss binding) :: [HsExpr GhcPs]
        forces = [() | HsApp _ function argument <- expressions,
            Just (Qual qualifier occurrence) <- [expressionVariable function],
            moduleNameString qualifier == "M", occNameString occurrence == "force",
            expressionVariable argument == Just parameter]
        cases = [() | HsCase _ _ matches <- expressions,
            let constructors = [occNameString (rdrNameOcc (unLoc name)) |
                    L _ alternative <- unLoc (mg_alts matches),
                    [L _ (ConPat _ name _)] <- [m_pats alternative]],
            all (`elem` constructors) ["NilT", "ConsT"]]
    unless (length forces == 1 && length cases == 1) $
        fail ("chooseM: expected one outer-list force and one case containing NilT and ConsT; found " ++
              show (length forces) ++ " forces and " ++ show (length cases) ++ " cases")

checkTerminalDoBlocks :: FilePath -> String -> IO ()
checkTerminalDoBlocks output target = do
    binding <- parseGeneratedBinding output target
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

    terminalExpression :: ExprLStmt GhcPs -> Maybe (LHsExpr GhcPs)
    terminalExpression statement = case unLoc statement of
        BodyStmt _ expression _ _ -> Just expression
        LastStmt _ expression _ _ -> Just expression
        _ -> Nothing

-- Parse the published artifact, including its extension pragmas, without loading
-- imports. Checks select a binding structurally, not by layout or fresh names.
parseGeneratedBinding :: FilePath -> String -> IO (Match GhcPs (LHsExpr GhcPs))
parseGeneratedBinding output target = do
    ast <- parseHaskellModule output
    case [unLoc alternative | L _ (ValD _ (FunBind _ name matches)) <- hsmodDecls ast,
            occNameString (rdrNameOcc (unLoc name)) == target,
            alternative <- unLoc (mg_alts matches)] of
        [binding] -> pure binding
        bindings -> fail (output ++ ": expected one equation for " ++ target ++
                          ", found " ++ show (length bindings))

collectAst :: (Data a, Data b) => b -> [a]
collectAst value = maybeToList (cast value) ++ concat (gmapQ collectAst value)

expressionVariable :: LHsExpr GhcPs -> Maybe RdrName
expressionVariable expression = case unLoc expression of
    HsVar _ name -> Just (unLoc name)
    HsPar _ nested -> expressionVariable nested
    ExprWithTySig _ nested _ -> expressionVariable nested
    _ -> Nothing

checkGeneratedWarnings :: FilePath -> IO ()
checkGeneratedWarnings root = do
    generated <- readFile (root </> "Generated.hs")
    checkGeneratedLayout "Generated.hs" generated
    unless (all (`isInfixOf` generated) ["capturedM x", "add z", "chooseM arg0", "nextEquation0", "partial'' <-"]) $
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
    writeFile (root </> warningLog) (out ++ err)
    unless (compiled == ExitSuccess) (fail (out ++ err))
    hlint <- findExecutable "hlint"
    case hlint of
        Nothing -> putStrLn "SKIP: generated code HLint check (hlint unavailable)"
        Just command -> do
            (linted, lintOut, lintErr) <- readProcessWithExitCode command [output] ""
            writeFile (root </> "hlint.log") (lintOut ++ lintErr)
            unless (linted == ExitSuccess) (fail (lintOut ++ lintErr))

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
