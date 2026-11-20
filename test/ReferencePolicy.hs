{-# LANGUAGE LambdaCase #-}
-- | A deterministic format gate, not a proof of semantic correctness or of
-- handwritten authorship. The shared runner owns comparison, samples and trials.
-- Parse both sides without loading them or depending on the target runtime.
-- Imports and compiler hooks are closed; name checks are conservative syntax
-- checks, not type inference or a security sandbox. Isolated builds check types.
module ReferencePolicy
    ( discoverReferenceCases, validateReferenceCases, checkReferencePolicyTests
    , diagnosticFixtures
    ) where

import Control.Monad (foldM, forM, unless, void, zipWithM)
import Data.Char (isAlphaNum, isSpace, toLower)
import Data.Data (Data, cast, gmapQ)
import Data.Foldable (toList)
import Data.List (isInfixOf, isPrefixOf, nub, sort, (\\))
import Data.Maybe (fromMaybe, isJust)
import GHC
    ( DynFlags, GhcPs, HsModule(..), HsDecl(..), HsBindLR(..), Sig(..)
    , HsType(..), HsSigType(..), HsWildCardBndrs(..), HsLocalBinds, HsLocalBindsLR(..), HsValBindsLR(..)
    , HsExpr(..), HsLit(..), LHsExpr, MatchGroup(..), Match(..), GRHSs(..), GRHS(..)
    , Pat(..), LPat, HsTupArg(..), HsDoFlavour(..), StmtLR(..), ExprLStmt
    , HsOverLit(..), OverLitVal(..), ArithSeqInfo(..)
    , IE(..), IEWrappedName(..), ImportDecl(..), ImportDeclQualifiedStyle(..)
    , IsBootInterface(..), ImportListInterpretation(..)
    , getLocA, getSessionDynFlags, moduleNameString, runGhc, unLoc
    , TyClDecl(..), tcdName, HsDataDefn(..), ConDecl(..), IEWildcard(..)
    )
import GHC.Data.Bag (bagToList)
import GHC.Data.FastString (mkFastString, unpackFS)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Ppr (showSDoc)
import GHC.Driver.Session (parseDynamicFilePragma)
import GHC.Hs.Utils (CollectFlag(..), collectHsBindBinders)
import GHC.Paths (libdir)
import qualified GHC.Parser as Parser
import GHC.Parser.Header (getOptions)
import GHC.Parser.Lexer (ParseResult(..), getPsErrorMessages, initParserState, unP)
import GHC.Types.Error (errMsgSpan, getMessages, isEmptyMessages)
import GHC.Types.Name.Occurrence (isVarOcc, occNameString)
import GHC.Types.Name.Reader (RdrName(..), rdrNameOcc)
import GHC.Types.PkgQual (RawPkgQual(..))
import GHC.Types.SrcLoc (GenLocated(..), mkRealSrcLoc)
import GHC.Types.SourceText (il_value)
import GHC.Utils.Outputable (ppr)
import Paths_cost_translate (getDataFileName)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, pathIsSymbolicLink)
import System.FilePath ((</>), makeRelative, splitDirectories, takeBaseName, takeDirectory, takeExtension, takeFileName)

diagnosticFixtures :: [(String, String)]
diagnosticFixtures =
    [("syntax", "source error"), ("type", "source error"), ("lambda", "unsupported")
    ,("unused-local-lambda", "unsupported expression")
    ,("unreachable-guard", "unsupported expression")
    ,("unused-vacuous-constructor-payload", "unsupported expression")
    ,("strict-semantics", "Strict"), ("recursive-value", "recursive value")
    ,("deriving", "deriving"), ("function-alias", "alias"), ("strict-field", "strict field")
    ,("structured-comparison", "strict scalar")
    ,("private-constructor-conversion", "export constructor Box")
    ,("private-second-constructor", "export constructor Second")
    ,("private-datatype", "export constructor Hidden")
    ,("pattern-bind-top", "unsupported pattern binding")
    ,("pattern-bind-local", "unsupported pattern binding")
    ,("local-signature-constraint", "unsupported type constraint")]

-- Only these fixture-policy inputs are infrastructure. A new prefix match is
-- not an exemption; keep this list in step with FixturePolicy's actual tests.
infrastructureFixtures :: [(String, [FilePath])]
infrastructureFixtures =
    [("fixture-policy-" ++ label, [file]) | (label, file) <-
        [("source-write", "Example.hs"), ("log-write", "Example.hs")
        ,("alias", "Example.hs"), ("multiline-log", "Example.hs")
        ,("append", "Example.hs"), ("open", "Example.hs")
        ,("probe-name", "OutputAstProbe.hs"), ("dynamic-path", "Example.hs")
        ,("partial", "Example.hs"), ("qualified-source", "Example.hs")
        ,("harmless", "Example.hs"), ("open-log", "Example.hs")
        ,("nested-writer", "Example.hs"), ("parse-error", "Example.hs")]]

sourceAliases :: [(String, [FilePath])]
sourceAliases =
    [("thunk-case", ["thunk-matching/Example.hs"])
    ,("thunk-guards", ["thunk-matching/Example.hs"])
    ,("scoped-factory-case-shadow-int", ["scoped-factory-case-shadow/Example.hs"])
    ,("scoped-factory-case-shadow-integer", ["scoped-factory-case-shadow/Example.hs"])
    ,("scoped-factory-guard-shadow-int", ["scoped-factory-guard-shadow/Example.hs"])
    ,("scoped-factory-guard-shadow-integer", ["scoped-factory-guard-shadow/Example.hs"])
    ,("constrained-sharing-same-dictionary", ["constrained-sharing-different-dictionaries/Example.hs"])
    ,("constrained-local-disjoint-scoped", ["constrained-local-disjoint-scoped/Example.hs", "constrained-local-disjoint-scoped/ClosedIntegral.hs"])
    ,("constrained-local-superclass-scoped", ["constrained-local-superclass-scoped/Example.hs", "constrained-local-superclass-scoped/AtIntegralOwner.hs"])
    ,("inferred-owner-residual", ["inferred-owner-residual/Example.hs", "inferred-owner-residual/UnrelatedShadow.hs"])
    ,("inferred-key-captured", ["inferred-key-captured/Example.hs"])
    ,("anf-scoped-renaming", ["anf-scoped-renaming/Shadowed.hs", "anf-scoped-renaming/Renamed.hs"])
    ,("polymorphic-opaque-producer", ["polymorphic-opaque-producer/Example.hs", "polymorphic-opaque-producer/HelperCollision.hs"])
    ,("top-level-function-interface", ["top-level-function-interface/Control.hs", "top-level-function-interface/WithValue.hs"])
    ,("generated-warnings-minimal", ["generated-warnings/Minimal.hs"])]

sourceOnlyDirectories :: [String]
sourceOnlyDirectories =
    ["thunk-matching", "scoped-factory-case-shadow", "scoped-factory-guard-shadow"]

-- These are the actual user-facing sources, not copies in test/cases. Keep one
-- reference case per example, and require its source and manual instructions even
-- when a runtime-free selector is the only requested test.
manualExamples :: [(String, FilePath)]
manualExamples =
    [("manual-thunk-arguments", "examples/ThunkArguments.hs")
    ,("manual-thunk-matching", "examples/ThunkMatching.hs")
    ,("manual-scoped-annotations", "examples/ScopedAnnotations.hs")
    ,("manual-constructor-equations", "examples/ConstructorEquations.hs")
    ,("manual-terminal-do", "examples/TerminalDo.hs")]

manualExampleProblems :: [FilePath] -> [(String, [FilePath])] -> String -> [String]
manualExampleProblems files cases instructions = concat
    [[problem source "missing manual example" | source `notElem` files] ++
     [problem source "manual example must be the registered semantic source"
        | lookup label cases /= Just [source]] ++
     [problem source "missing source/selector row in examples/README.md"
         | not (any (\line -> all (`isInfixOf` line) [takeFileName source, label])
                    (lines instructions))]
    | (label, source) <- manualExamples]

checkManualExamples :: [(String, [FilePath])] -> IO [String]
checkManualExamples cases = do
    files <- fmap concat $ forM manualExamples $ \(_, source) -> do
        path <- getDataFileName source
        exists <- doesFileExist path
        symbolic <- if exists then pathIsSymbolicLink path else pure False
        pure [source | exists && not symbolic]
    instructionsPath <- getDataFileName "examples/README.md"
    exists <- doesFileExist instructionsPath
    instructions <- if exists then readFile instructionsPath else pure ""
    pure (manualExampleProblems files cases instructions)

excludedFixtures :: [(String, [FilePath])]
excludedFixtures = infrastructureFixtures ++
    [("diagnostics-" ++ label, ["Example.hs"]) | (label, _) <- diagnosticFixtures] ++
    [("whole-module-failure", ["Example.hs"])]

-- Files and directories are relative to the scanned root. Symlinks never become
-- invisible imports or let an inventory walk escape that root.
data Inventory = Inventory [FilePath] [FilePath] [String]

readInventory :: FilePath -> IO Inventory
readInventory root = do
    exists <- doesDirectoryExist root
    if exists then walk "" else pure (Inventory [] [] [problem root "missing fixture directory"])
  where
    walk relative = do
        names <- sort <$> listDirectory (root </> relative)
        entries <- forM names $ \name -> do
            let file = relative </> name
                full = root </> file
            symbolic <- pathIsSymbolicLink full
            directory <- doesDirectoryExist full
            if symbolic then pure (Inventory [] [] [problem file "symlinks are not permitted"])
            else if directory then do
                Inventory dirs files problems <- walk file
                pure (Inventory (file:dirs) files problems)
            else do
                regular <- doesFileExist full
                pure (if regular then Inventory [] [file] []
                      else Inventory [] [] [problem file "unrecognized filesystem entry"])
        pure (Inventory (concat [ds | Inventory ds _ _ <- entries])
                        (concat [fs | Inventory _ fs _ <- entries])
                        (concat [ps | Inventory _ _ ps <- entries]))

-- The same inventory checker serves production and the checked-in miniature
-- inventories. Production additionally requires every fixed exception/alias.
inventoryCases :: Bool -> Inventory -> ([(String, [FilePath])], [String])
inventoryCases complete (Inventory directories files filesystemProblems) = (cases, sort problems)
  where
    topDirectories = [d | d <- directories, length (splitDirectories d) == 1]
    fixed = map fst excludedFixtures
    reserved label = any (`isPrefixOf` label) ["diagnostics-", "fixture-policy-"]
    unregistered = [d | d <- topDirectories, reserved d, d `notElem` fixed]
    labels = sort . nub $ [d | d <- topDirectories,
        d `notElem` fixed ++ sourceOnlyDirectories ++ unregistered] ++
        [label | complete, (label, _) <- sourceAliases]
    sources label = case [source | (registered, source) <- manualExamples, registered == label] of
        [] -> map ("test/cases" </>) . sort $
            fromMaybe [label </> "Example.hs"] (lookup label sourceAliases)
        examples -> examples
    cases = [(label, sources label) | label <- labels]
    semanticFiles = concat [[label </> "Case.hs", label </> "Reference.hs"] ++
                           [makeRelative "test/cases" source | source <- paths,
                            splitDirectories "test/cases" `isPrefixOf` splitDirectories source]
                           | (label, paths) <- cases]
    fixedFiles = [label </> file | (label, paths) <- excludedFixtures,
                                  complete || label `elem` topDirectories, file <- paths]
    providerFiles = [label </> "Example.hs" | label <- sourceOnlyDirectories,
                     complete || label `elem` topDirectories]
    expected = nub (semanticFiles ++ fixedFiles ++ providerFiles)
    requiredDirs = [d | complete, d <- fixed ++ sourceOnlyDirectories ++ map fst sourceAliases ++
                                      [label | (label, _) <- manualExamples]]
    problems = filesystemProblems ++
        [problem d "unregistered diagnostic or infrastructure fixture" | d <- unregistered] ++
        [problem d "unaccounted nested fixture directory" | d <- directories,
            length (splitDirectories d) /= 1] ++
        [problem d "missing registered fixture directory" | d <- requiredDirs, d `notElem` topDirectories] ++
        [problem file "missing required semantic source, Case.hs, or Reference.hs"
            | file <- sort expected, file `notElem` files] ++
        [problem file (unexpected file) | file <- sort files, file `notElem` expected]
    unexpected file
        | last (splitDirectories file) == "Main.hs" = "custom Main.hs is forbidden; the shared observer is the only entrypoint"
        | last (splitDirectories file) == "Regression.hs" = "Regression.hs is not permitted in the final semantic inventory"
        | takeExtension file == ".hs" = "unaccounted Haskell source (extra local modules are forbidden)"
        | otherwise = "unaccounted fixture file"

discoverReferenceCases :: IO [(String, [FilePath])]
discoverReferenceCases = do
    root <- getDataFileName "test/cases"
    inventory <- readInventory root
    let (cases, inventoryProblems) = inventoryCases True inventory
    flags <- runGhc (Just libdir) getSessionDynFlags
    sourceProblems <- fmap concat $ forM cases $ \(label, _) -> do
        let directory = "test/cases" </> label
        reference <- parseExisting flags ReferenceFile (directory </> "Reference.hs")
        caseModule <- parseExisting flags CaseFile (directory </> "Case.hs")
        let generated = maybe emptyGeneratedNames (either (const emptyGeneratedNames) (generatedNames . snd)) reference
            referenceProblems = maybe [] (either id (\(fileFlags, ast) ->
                checkModule fileFlags ReferenceFile emptyGeneratedNames (directory </> "Reference.hs") ast)) reference
            caseProblems = maybe [] (either id (\(fileFlags, ast) ->
                checkModule fileFlags CaseFile generated (directory </> "Case.hs") ast)) caseModule
        pure (referenceProblems ++ caseProblems)
    registrationProblems <- checkRegistrations
    exampleProblems <- checkManualExamples cases
    failProblems (inventoryProblems ++ sourceProblems ++ registrationProblems ++ exampleProblems)
    pure cases

validateReferenceCases :: IO ()
validateReferenceCases = void discoverReferenceCases

data FileRole = CaseFile | ReferenceFile deriving Eq

problem :: FilePath -> String -> String
problem file message = file ++ ": reference policy: " ++ message

failProblems :: [String] -> IO ()
failProblems problems = unless (null problems) (fail (unlines (sort (nub problems))))

parseExisting :: DynFlags -> FileRole -> FilePath -> IO (Maybe (Either [String] (DynFlags, HsModule GhcPs)))
parseExisting flags role file = do
    path <- getDataFileName file
    exists <- doesFileExist path
    if exists then Just <$> parseFile flags role file else pure Nothing

parseFile :: DynFlags -> FileRole -> FilePath -> IO (Either [String] (DynFlags, HsModule GhcPs))
parseFile flags role file = do
    path <- getDataFileName file
    source <- readFile path
    let preflight = pragmaProblems role file source
        buffer = stringToStringBuffer source
        (headerErrors, options) = getOptions (initParserOpts flags) buffer file
    if not (null preflight) then pure (Left preflight)
    else if not (isEmptyMessages headerErrors) then pure (Left [problem file "cannot parse file pragmas"])
    else do
        -- All options were checked before passing them to GHC. This never loads
        -- plugins, preprocessors, imports, foreign code or Template Haskell.
        (fileFlags, leftovers, _) <- parseDynamicFilePragma flags options
        if not (null leftovers) then pure (Left [problem file "unrecognized file options"])
        else pure $ case unP Parser.parseModule (initParserState (initParserOpts fileFlags)
                         buffer (mkRealSrcLoc (mkFastString file) 1 1)) of
            PFailed failed -> Left (parseProblems fileFlags failed)
            POk parsedState _ | not (isEmptyMessages (getPsErrorMessages parsedState)) ->
                Left (parseProblems fileFlags parsedState)
            POk _ parsed -> Right (fileFlags, unLoc parsed)
  where
    parseProblems fileFlags state =
        let location = case bagToList (getMessages (getPsErrorMessages state)) of
                message:_ -> showSDoc fileFlags (ppr (errMsgSpan message))
                [] -> file
        in [problem location "cannot parse Haskell input"]

-- Scan every pragma, not only GHC's header pragmas. The deliberately bounded
-- format also rejects pragma-looking text in strings/comments conservatively.
pragmaProblems :: FileRole -> FilePath -> String -> [String]
pragmaProblems role file source =
    [problem file "CPP directives are forbidden" | any directive (lines source)] ++
    concatMap checkPragma (pragmaBodies source)
  where
    directive line = "#" `isPrefixOf` dropWhile isSpace line
    checkPragma body = case words body of
        "LANGUAGE":rest ->
            [problem file ("forbidden or unsupported LANGUAGE extension " ++ extension)
            | extension <- words (map (\c -> if c == ',' then ' ' else c) (unwords rest)),
              extension `notElem` permittedExtensions ||
              (role == CaseFile && extension `elem` ["OverloadedStrings", "NoImplicitPrelude"])]
        "OPTIONS_GHC":rest | not (null rest), all (`elem` harmlessOptions) rest -> []
        _ -> [problem file "only whitelisted LANGUAGE pragmas and harmless warning options are permitted; no compiler hooks"]
    harmlessOptions = ["-Wno-orphans", "-Wno-simplifiable-class-constraints"]

pragmaBodies :: String -> [String]
pragmaBodies [] = []
pragmaBodies source@(_:remaining)
    | "{-#" `isPrefixOf` source =
        let (body, rest) = untilClose (drop 3 source)
        in body : pragmaBodies rest
    | otherwise = pragmaBodies remaining
  where
    untilClose [] = ("", "")
    untilClose input@(first:more)
        | "#-}" `isPrefixOf` input = ("", drop 3 input)
        | otherwise = let (body, rest) = untilClose more in (first:body, rest)

permittedExtensions :: [String]
permittedExtensions =
    ["Haskell2010", "GHC2021", "GHC2024", "AllowAmbiguousTypes", "BangPatterns"
    ,"BlockArguments", "ConstraintKinds", "DataKinds", "DeriveAnyClass", "DeriveDataTypeable"
    ,"DeriveFoldable", "DeriveFunctor", "DeriveGeneric", "DeriveTraversable", "DerivingStrategies"
    ,"EmptyCase", "EmptyDataDecls", "EmptyDataDeriving", "ExplicitForAll", "ExplicitNamespaces"
    ,"FlexibleContexts", "FlexibleInstances", "GADTs", "GADTSyntax", "GeneralizedNewtypeDeriving"
    ,"ImportQualifiedPost", "InstanceSigs", "KindSignatures", "LambdaCase", "MagicHash"
    ,"MultiParamTypeClasses", "MultiWayIf", "NamedFieldPuns", "NoImplicitPrelude"
    ,"NoMonomorphismRestriction", "NoMonoLocalBinds", "NumericUnderscores", "OverloadedStrings"
    ,"PolyKinds", "QuantifiedConstraints", "RankNTypes", "RecordWildCards", "RoleAnnotations"
    ,"ScopedTypeVariables", "StandaloneDeriving", "StandaloneKindSignatures", "TupleSections"
    ,"TypeApplications", "TypeFamilies", "TypeFamilyDependencies", "TypeOperators"
    ,"UndecidableInstances", "UndecidableSuperClasses", "ViewPatterns"]

checkModule :: DynFlags -> FileRole -> GeneratedNames -> FilePath -> HsModule GhcPs -> [String]
checkModule flags role generated file ast =
    [at ("must declare module " ++ expectedModule) | moduleName /= Just expectedModule] ++
    concatMap checkImport (hsmodImports ast) ++
    concatMap checkDeclaration (hsmodDecls ast) ++
    [at ("forbidden IO, unsafe, runner, property, or trial-setting name " ++ name)
        | name <- sort . nub $ map (occNameString . rdrNameOcc) (collectNames ast), forbiddenName name] ++
    [at ("source value calls are forbidden in Reference.hs: " ++ moduleNameString qualifier ++ "." ++ occNameString occurrence)
        | role == ReferenceFile, Qual qualifier occurrence <- nub (collectNames ast),
          moduleNameString qualifier `elem` sourceQualifiers, isVarOcc occurrence] ++
    if role == CaseFile then checkCatalog file ast else []
  where
    at = problem file
    expectedModule = if role == CaseFile then "Case" else "Generated"
    moduleName = moduleNameString . unLoc <$> hsmodName ast
    imports = map unLoc (hsmodImports ast)
    names = map (moduleNameString . unLoc . ideclName) imports
    -- Minimal is the declared source of this one explicit source alias, not an
    -- arbitrary second local module. The paired ANF sources both declare Example.
    sourceModule = fromMaybe defaultSourceModule $ lookup (takeDirectory file)
        [("test/cases" </> label, takeBaseName source) | (label, source) <- manualExamples]
    defaultSourceModule = if file `elem` ["test/cases/generated-warnings-minimal/Case.hs",
                                         "test/cases/generated-warnings-minimal/Reference.hs"]
                          then "Minimal" else "Example"
    sourceQualifiers = [maybe sourceModule (moduleNameString . unLoc) (ideclAs declaration)
                       | declaration <- imports, moduleNameString (unLoc (ideclName declaration)) == sourceModule]
    requiredImports = if role == CaseFile then ["Generated", "ReferenceSupport"] else []
    checkImport located =
        let declaration = unLoc located
            name = moduleNameString (unLoc (ideclName declaration))
            location = showSDoc flags (ppr (getLocA located))
            allowed = externalModule name || name == sourceModule || name == "DemandSupport" ||
                (name == "Test.QuickCheck" && generatorImport declaration) ||
                (role == CaseFile && name `elem` ["Generated", "ReferenceSupport"])
            plainSource = name == sourceModule && ideclQualified declaration == NotQualified
            packageImport = case ideclPkgQual declaration of NoRawPkgQual -> False; _ -> True
            alias = moduleNameString . unLoc <$> ideclAs declaration
            modifiedSupport = role == CaseFile && name `elem` ["ReferenceSupport", "Prelude"] &&
                (ideclQualified declaration /= NotQualified || isJust (ideclAs declaration) ||
                 case ideclImportList declaration of
                    Nothing -> False
                    Just (Exactly, _) -> name == "Prelude"
                    _ -> True)
        in [problem location ("forbidden import " ++ name ++ "; only the declared source, runtime, support, and whitelisted base modules are allowed") | not allowed] ++
           [problem location (sourceModule ++ " imports must be qualified and used only for source types/constructors in references") | plainSource] ++
           [problem location "package-qualified and SOURCE imports are forbidden" | packageImport || ideclSource declaration /= NotBoot] ++
           [problem location "Case must import ReferenceSupport/Prelude without qualification, aliases, or hiding; Prelude must remain unmodified" | modifiedSupport] ++
           [problem location "reserved module alias" | Just selected <- [alias],
                selected `elem` ["ReferenceSupport", "Generated", "DemandSupport"], selected /= name] ++
           [problem location "Generated () is forbidden; semantic probes must use generated functions" |
                name == "Generated", Just (Exactly, entries) <- [ideclImportList declaration], null (unLoc entries)]
    generatorImport declaration = case ideclImportList declaration of
        Just (Exactly, entries) -> all ((`elem` generatorNames) . occNameString . rdrNameOcc) (collectNames entries)
        Nothing | ideclQualified declaration /= NotQualified ->
            let qualifier = maybe "Test.QuickCheck" (moduleNameString . unLoc) (ideclAs declaration)
            in all (`elem` generatorNames) [occNameString occurrence | Qual selected occurrence <- collectNames ast,
                                          moduleNameString selected == qualifier]
        _ -> False
    checkDeclaration located = case unLoc located of
        ForD {} -> [at "foreign declarations are forbidden"]
        SpliceD {} -> [at "Template Haskell declarations are forbidden"]
        RuleD {} -> [at "rewrite rules are forbidden"]
        AnnD {} -> [at "compiler annotations are forbidden"]
        _ -> []
    -- Required imports are checked even when the module has no imports at all.
    checkCatalog file' moduleAst =
        [problem file' ("missing required import " ++ name) | name <- requiredImports, name `notElem` names] ++
        catalogProblems generated file' moduleAst

collectNames :: Data a => a -> [RdrName]
collectNames value = case cast value of
    Just name -> [name]
    Nothing -> concat (gmapQ collectNames value)

forbiddenName :: String -> Bool
forbiddenName name = "unsafe" `isPrefixOf` map toLower name ||
    "quickCheck" `isPrefixOf` name || "verboseCheck" `isPrefixOf` name || name `elem`
    ["IO", "main", "Property", "property", "Testable", "runProbes", "runProbe"
    ,"readFile", "writeFile", "appendFile", "readLn", "readIO", "interact"
    ,"getLine", "getChar", "getContents", "putStr", "putStrLn", "putChar", "print", "ioError"
    ,"maxSuccess", "maxDiscardRatio", "maxSize", "maxShrinks", "replay", "chatty"
    ,"stdArgs", "withMaxSuccess", "withMaxShrinks", "expectFailure"
    ,"forAll", "forAllShrink", "conjoin", "disjoin", "counterexample", "ioProperty", "monadicIO"
    ,"runIO", "foreign", "runQ", "addDependentFile", "drawDemands", "replayDemands"]

generatorNames :: [String]
generatorNames = ["Gen", "Arbitrary", "arbitrary", "shrink", "sized", "resize", "scale"
    ,"choose", "chooseInt", "chooseInteger", "elements", "oneof", "frequency", "vectorOf"
    ,"listOf", "listOf1", "suchThat", "suchThatMaybe", "shrinkList", "shrinkIntegral"
    ,"arbitraryBoundedIntegral"]

-- Data.* is not an open namespace: these are base modules, not arbitrary
-- installed packages or unchecked local helpers with a convenient prefix.
externalModule :: String -> Bool
externalModule name =
    ("Memorist." `isPrefixOf` name && not (any (`elem` splitModule name) ["Unsafe", "Internal"])) ||
    name `elem`
    ["Prelude", "Control.Applicative", "Control.Monad", "Control.Monad.Fix", "Control.Monad.Zip"
    ,"Data.Bifunctor", "Data.Bifoldable", "Data.Bitraversable", "Data.Bits", "Data.Bool"
    ,"Data.Char", "Data.Coerce", "Data.Complex", "Data.Data", "Data.Either", "Data.Eq"
    ,"Data.Fixed", "Data.Foldable", "Data.Function", "Data.Functor", "Data.Functor.Classes"
    ,"Data.Functor.Compose", "Data.Functor.Const", "Data.Functor.Contravariant"
    ,"Data.Functor.Identity", "Data.Functor.Product", "Data.Functor.Sum", "Data.Int", "Data.Kind"
    ,"Data.List", "Data.List.NonEmpty", "Data.Maybe", "Data.Monoid", "Data.Ord", "Data.Proxy"
    ,"Data.Ratio", "Data.Semigroup", "Data.String", "Data.Traversable", "Data.Tuple"
    ,"Data.Type.Coercion", "Data.Type.Equality", "Data.Typeable", "Data.Void", "Data.Word"
    ,"Type.Reflection", "GHC.TypeLits"]
  where
    splitModule input = case break (== '.') input of
        (part, []) -> [part]
        (part, _:rest) -> part : splitModule rest

data GeneratedNames = GeneratedNames [String] [String] [(String, String)]

emptyGeneratedNames :: GeneratedNames
emptyGeneratedNames = GeneratedNames [] [] []

generatedNames :: HsModule GhcPs -> GeneratedNames
generatedNames ast = GeneratedNames (filter exported functions) (filter exported shapes)
    [identity | identity <- declaredConstructors ast,
                maybe True (any (constructorIncluded shapes identity . unLoc) . unLoc) (hsmodExports ast)]
  where
    functions = [occNameString (rdrNameOcc name) | ValD _ binding <- map unLoc (hsmodDecls ast),
                 name <- collectHsBindBinders CollNoDictBinders binding, isVarOcc (rdrNameOcc name)]
    shapes = [occNameString (rdrNameOcc (tcdName declaration)) |
              TyClD _ declaration@DataDecl {} <- map unLoc (hsmodDecls ast)]
    exported name = case hsmodExports ast of
        Nothing -> True
        Just entries -> name `elem` map (occNameString . rdrNameOcc) (collectNames entries)

-- Retain the parent type for T(..), but never confuse a bare T export with
-- its same-spelled data constructor. Only constructor declarations supply values.
declaredConstructors :: HsModule GhcPs -> [(String, String)]
declaredConstructors ast =
    [(occNameString (rdrNameOcc (tcdName declaration)), occNameString (rdrNameOcc (unLoc name)))
    | TyClD _ declaration@DataDecl { tcdDataDefn = definition } <- map unLoc (hsmodDecls ast),
      constructor <- toList (dd_cons definition),
      name <- case unLoc constructor of
          ConDeclH98 { con_name = single } -> [single]
          ConDeclGADT { con_names = multiple } -> toList multiple]

constructorIncluded :: [String] -> (String, String) -> IE GhcPs -> Bool
constructorIncluded types (parent, constructor) entry = case entry of
    IEVar _ name _ -> wrappedName name == Just constructor
    IEThingAbs _ name _ -> constructor `notElem` types && wrappedName name == Just constructor
    IEThingAll _ name _ -> wrappedName name == Just parent
    IEThingWith _ name wildcard children _ ->
        wrappedName name == Just parent &&
        (wildcard /= NoIEWildcard || any ((== Just constructor) . wrappedName) children)
    _ -> False
  where
    wrappedName (L _ (IEName _ name)) = Just (occNameString (rdrNameOcc (unLoc name)))
    wrappedName _ = Nothing

catalogProblems :: GeneratedNames -> FilePath -> HsModule GhcPs -> [String]
catalogProblems (GeneratedNames generatedFunctions generatedShapes generatedConstructors) file ast =
    exportProblems ++ signatureProblems ++ bindingProblems ++ shadowProblems
  where
    at = problem file
    declarations = map unLoc (hsmodDecls ast)
    exportProblems = [at "Case must explicitly export only probes" | not validExports]
    validExports = case hsmodExports ast of
        Just exports -> case unLoc exports of
            [L _ (IEVar _ (L _ (IEName _ name)) _)] -> simpleName (unLoc name) == Just "probes"
            _ -> False
        Nothing -> False
    signatures = [signature | SigD _ (TypeSig _ names signature) <- declarations,
                              any ((== "probes") . occNameString . rdrNameOcc . unLoc) names]
    signatureProblems = [at "probes must have the explicit static signature probes :: [Probe]"
                        | not (case signatures of [signature] -> probeListType signature; _ -> False)]
    bindings = [binding | ValD _ binding@FunBind { fun_id = name } <- declarations,
                          simpleName (unLoc name) == Just "probes"]
    environment = extendBindings [binding | ValD _ binding <- declarations] (catalogImports ast)
    bindingProblems = case bindings of
        [binding] -> case bindingBody binding of
            Just ([], _, _) -> case lookup "probes" environment >>= either (const Nothing) Just . catalogEntries of
                Nothing -> case lookup "probes" environment of
                    Just value -> either (\reason -> [at ("unsupported static probe catalog: " ++ reason)]) (const []) (catalogEntries value)
                    Nothing -> [at "missing probes binding"]
                Just entries ->
                    let labels = [label | StaticProbe _ label _ <- entries]
                        successful = [entry | entry@(StaticProbe kind _ _) <- entries, kind `notElem` ["snapshot", "failure"]]
                        generated entry = any (reaches False (fromGenerated generatedFunctions ast)) (programFields entry)
                        constructed entry = any (reachesSyntax True False (fromGeneratedConstructor generatedShapes generatedConstructors ast)) (programFields entry)
                        conversion entry = any (reaches False (runtimeConversion ast)) (programFields entry) &&
                            any (reaches True (fromGenerated generatedShapes ast)) (allFields entry)
                    in [at "probe catalog must contain a successful costed computation; failures and snapshots are supplementary" | null successful] ++
                       [at "probe labels must be nonempty static strings" | any null labels] ++
                       [at "probe labels must be unique" | length (nub labels) /= length labels] ++
                        [at ("costed callback has no reachable Generated function, exported constructor, or generated-instance conversion: " ++ label)
                           | entry@(StaticProbe _ label _) <- successful, not (generated entry || constructed entry || conversion entry)]
            _ -> [at "probes must be one unguarded, argument-free static catalog"]
        _ -> [at "Case must define exactly one top-level static probes catalog"]
    shadowProblems =
        [at ("cannot redefine ReferenceSupport catalog name " ++ name)
        | name <- nub (boundNames ast ++ concatMap declaredTypeNames declarations),
          name `elem` ("Probe":"Sample":"++":"$":map fst probeArities)]
    declaredTypeNames (TyClD _ declaration) = [occNameString (rdrNameOcc (tcdName declaration))]
    declaredTypeNames _ = []
    probeListType (HsWC _ (L _ (HsSig _ _ body))) = case unLoc body of
        HsListTy _ (L _ (HsTyVar _ _ name)) -> supportName (unLoc name) == Just "Probe"
        _ -> False
    probeListType _ = False

boundNames :: Data a => a -> [String]
boundNames value = names ++ concat (gmapQ boundNames value)
  where
    names = case cast value :: Maybe (HsBindLR GhcPs GhcPs) of
        Just binding -> map (occNameString . rdrNameOcc) (collectHsBindBinders CollNoDictBinders binding)
        Nothing -> case cast value :: Maybe (Sig GhcPs) of
            Just (ClassOpSig _ _ methods _) -> map (occNameString . rdrNameOcc . unLoc) methods
            _ -> []

simpleName :: RdrName -> Maybe String
simpleName (Unqual occurrence) = Just (occNameString occurrence)
simpleName _ = Nothing

supportName :: RdrName -> Maybe String
supportName (Qual qualifier occurrence) | moduleNameString qualifier == "ReferenceSupport" = Just (occNameString occurrence)
supportName name = simpleName name

probeArities :: [(String, Int)]
probeArities = [("scalar", 2), ("scalarSource", 3), ("boolean", 2), ("integer", 2),
                ("demanded", 4), ("snapshot", 2), ("failure", 2), ("generatedDemands", 1)]

-- A small, bounded interpreter for catalog *structure*, never for computations.
-- Program callbacks remain syntax plus lexical bindings. Finite tables, tuple
-- patterns, helper constructors, comprehensions and static conditionals work;
-- unknown calls, recursion, unbounded ranges and sample-dependent labels fail.
type Environment = [(String, StaticValue)]

data StaticValue
    = Syntax (LHsExpr GhcPs) Environment
    | Alias String StaticValue
    | Function [LPat GhcPs] (LHsExpr GhcPs) (HsLocalBinds GhcPs) Environment
    | OpaqueBinding (HsBindLR GhcPs GhcPs) Environment
    | Builtin String [StaticValue]
    | StaticList [StaticValue]
    | StaticTuple [StaticValue]
    | StaticText String
    | StaticBool Bool
    | StaticNumber Integer
    | StaticProbe String String [StaticValue]
    | StaticDemand String [StaticValue]
    | Unknown

catalogBound :: Int
catalogBound = 20000

catalogEntries :: StaticValue -> Either String [StaticValue]
catalogEntries value = staticList 300 value >>= mapM entry
  where
    entry item = forceStatic 300 item >>= \result -> case result of
        StaticProbe {} -> Right result
        _ -> Left "catalog entries must resolve to ReferenceSupport probe constructors"

bindingBody :: HsBindLR GhcPs GhcPs -> Maybe ([LPat GhcPs], LHsExpr GhcPs, HsLocalBinds GhcPs)
bindingBody FunBind { fun_matches = MG { mg_alts = alternatives } } = case unLoc alternatives of
    [L _ Match { m_pats = patterns, m_grhss = GRHSs { grhssGRHSs = [L _ (GRHS _ [] body)],
                                                    grhssLocalBinds = locals } }] -> Just (patterns, body, locals)
    _ -> Nothing
bindingBody _ = Nothing

extendBindings :: [HsBindLR GhcPs GhcPs] -> Environment -> Environment
extendBindings bindings outer = environment
  where
    environment = [(occNameString (rdrNameOcc name), value binding) | binding <- bindings,
                   name <- collectHsBindBinders CollNoDictBinders binding] ++ outer
    value binding = Alias (bindingLocation binding) $ case bindingBody binding of
        Just (patterns, body, locals) -> Function patterns body locals environment
        Nothing -> OpaqueBinding binding environment
    bindingLocation FunBind { fun_id = name } = nameKey (unLoc name) ++ show (getLocA name)
    bindingLocation PatBind { pat_lhs = pattern' } = show (getLocA pattern')
    bindingLocation _ = "unsupported-binding"

localBindings :: HsLocalBinds GhcPs -> [HsBindLR GhcPs GhcPs]
localBindings (HsValBinds _ (ValBinds _ bindings _)) = map unLoc (bagToList bindings)
localBindings _ = []

catalogImports :: HsModule GhcPs -> Environment
catalogImports ast = concatMap (imported . unLoc) (hsmodImports ast) ++
    [(prefix ++ name, Builtin name []) | name <- staticBuiltins, prefix <- ["", "Prelude."]]
  where
    imported declaration =
        let name = moduleNameString (unLoc (ideclName declaration))
            qualifier = maybe name (moduleNameString . unLoc) (ideclAs declaration)
            methods
                | name == "ReferenceSupport" = map fst probeArities
                | name == "DemandSupport" = ["demandProbe"]
                | otherwise = []
        in [(prefix ++ method, Builtin method []) | method <- methods, importIncludes declaration method,
            prefix <- (qualifier ++ ".") : ["" | ideclQualified declaration == NotQualified]]

importIncludes :: ImportDecl GhcPs -> String -> Bool
importIncludes declaration name = case ideclImportList declaration of
    Nothing -> True
    Just (interpretation, entries) ->
        let included = name `elem` map (occNameString . rdrNameOcc) (collectNames entries)
        in if interpretation == Exactly then included else not included

staticBuiltins :: [String]
staticBuiltins = ["++", ":", "$", "concat", "concatMap", "map", "filter", "take", "drop"
    ,"zip", "zipWith", "replicate", "reverse", "fst", "snd", "id", "const", "not", "&&", "||", "show"]

nameKey :: RdrName -> String
nameKey (Qual qualifier occurrence) = moduleNameString qualifier ++ "." ++ occNameString occurrence
nameKey name = occNameString (rdrNameOcc name)

forceStatic :: Int -> StaticValue -> Either String StaticValue
forceStatic fuel _ | fuel <= 0 = Left "catalog structure exceeds the finite expansion bound"
forceStatic fuel (Alias _ value) = forceStatic (fuel - 1) value
forceStatic fuel (Function [] body locals environment) =
    forceStatic (fuel - 1) (Syntax body (extendBindings (localBindings locals) environment))
forceStatic fuel (Syntax located environment) = case unLoc located of
    HsPar _ body -> next body
    ExprWithTySig _ body _ -> next body
    HsVar _ name -> case nameKey (unLoc name) of
        "True" -> Right (StaticBool True)
        "False" -> Right (StaticBool False)
        "[]" -> Right (StaticList [])
        key -> maybe (Left ("unknown static catalog name " ++ key ++ "; probe constructors must come from ReferenceSupport"))
                     (forceStatic (fuel - 1)) (lookup key environment)
    HsLit _ (HsString _ text) -> Right (StaticText (unpackFS text))
    HsOverLit _ OverLit { ol_val = HsIntegral number } -> Right (StaticNumber (il_value number))
    NegApp _ body _ -> StaticNumber . negate <$> staticNumber (fuel - 1) (Syntax body environment)
    ExplicitList _ values -> checkedList (map (`Syntax` environment) values)
    ExplicitTuple _ values _ -> StaticTuple <$> mapM tuplePart values
    HsIf _ condition yes no -> do
        flag <- staticBool (fuel - 1) (Syntax condition environment)
        next (if flag then yes else no)
    HsLet _ locals body -> forceStatic (fuel - 1) (Syntax body (extendBindings (localBindings locals) environment))
    HsLam _ _ (MG _ alternatives) -> case unLoc alternatives of
        [L _ Match { m_pats = patterns, m_grhss = GRHSs { grhssGRHSs = [L _ (GRHS _ [] body)],
                                                        grhssLocalBinds = locals } }] -> Right (Function patterns body locals environment)
        _ -> Left "unsupported static catalog lambda"
    HsApp _ function argument -> do
        callable <- next function
        applyStatic (fuel - 1) callable (Syntax argument environment)
    OpApp _ left operator right -> do
        callable <- next operator
        partial <- applyStatic (fuel - 1) callable (Syntax left environment)
        applyStatic (fuel - 1) partial (Syntax right environment)
    HsDo _ ListComp statements -> StaticList <$> comprehension (fuel - 1) environment (unLoc statements)
    ArithSeq _ _ (FromTo start end) -> range start Nothing end
    ArithSeq _ _ (FromThenTo start step end) -> range start (Just step) end
    _ -> Left "catalog must use finite static lists/combinators, not computed labels or sample-dependent catalogs"
  where
    next body = forceStatic (fuel - 1) (Syntax body environment)
    tuplePart (Present _ body) = Right (Syntax body environment)
    tuplePart _ = Left "tuple sections are not static catalog tables"
    range start step end = do
        first <- staticNumber fuel (Syntax start environment)
        lastNumber <- staticNumber fuel (Syntax end environment)
        values <- case step of
            Nothing -> Right [first .. lastNumber]
            Just expression -> do
                second <- staticNumber fuel (Syntax expression environment)
                Right [first, second .. lastNumber]
        checkedList (map StaticNumber values)
forceStatic _ OpaqueBinding {} = Left "unsupported or recursive static catalog helper"
forceStatic _ Unknown = Left "sample-dependent or unresolved catalog value"
forceStatic _ value = Right value

checkedList :: [StaticValue] -> Either String StaticValue
checkedList values
    | length (take (catalogBound + 1) values) > catalogBound = Left "static catalog exceeds its finite size bound"
    | otherwise = Right (StaticList values)

staticList :: Int -> StaticValue -> Either String [StaticValue]
staticList fuel value = forceStatic fuel value >>= \case
    StaticList entries -> Right entries
    _ -> Left "expected a finite static catalog list"

staticText :: Int -> StaticValue -> Either String String
staticText fuel value = forceStatic fuel value >>= \case
    StaticText text | length (take (catalogBound + 1) text) <= catalogBound -> Right text
    _ -> Left "probe labels must be finite static strings, not sample-dependent/computed labels"

staticBool :: Int -> StaticValue -> Either String Bool
staticBool fuel value = forceStatic fuel value >>= \case
    StaticBool flag -> Right flag
    _ -> Left "catalog conditions must be static booleans"

staticNumber :: Int -> StaticValue -> Either String Integer
staticNumber fuel value = forceStatic fuel value >>= \case
    StaticNumber number -> Right number
    _ -> Left "catalog bounds must be static integers"

applyStatic :: Int -> StaticValue -> StaticValue -> Either String StaticValue
applyStatic fuel _ _ | fuel <= 0 = Left "catalog helper expansion exceeds its finite bound"
applyStatic fuel (Function (pattern':rest) body locals environment) argument = do
    bindings <- matchStatic (fuel - 1) pattern' argument
    forceStatic (fuel - 1) (Function rest body locals (bindings ++ environment))
applyStatic fuel (Builtin name arguments) argument = builtinStatic (fuel - 1) name (arguments ++ [argument])
applyStatic fuel function argument = do
    resolved <- forceStatic (fuel - 1) function
    case resolved of
        Function {} -> applyStatic (fuel - 1) resolved argument
        Builtin {} -> applyStatic (fuel - 1) resolved argument
        _ -> Left "unsupported static catalog function"

matchStatic :: Int -> LPat GhcPs -> StaticValue -> Either String Environment
matchStatic fuel (L _ pattern') value = case pattern' of
    WildPat _ -> Right []
    VarPat _ name -> Right [(nameKey (unLoc name), Alias (show (getLocA name)) value)]
    ParPat _ inner -> matchStatic fuel inner value
    LazyPat _ inner -> matchStatic fuel inner value
    BangPat _ inner -> matchStatic fuel inner value
    SigPat _ inner _ -> matchStatic fuel inner value
    AsPat _ name inner -> ((nameKey (unLoc name), Alias (show (getLocA name)) value) :) <$> matchStatic fuel inner value
    TuplePat _ patterns _ -> do
        result <- forceStatic fuel value
        case result of
            StaticTuple entries | length entries == length patterns ->
                concat <$> zipWithM (matchStatic (fuel - 1)) patterns entries
            _ -> Left "catalog tuple pattern does not match its static table"
    ListPat _ patterns -> do
        entries <- staticList fuel value
        if length entries /= length patterns then Left "catalog list pattern does not match its static table"
        else concat <$> zipWithM (matchStatic (fuel - 1)) patterns entries
    _ -> Left "catalog helpers support variable, tuple and list patterns only"

builtinStatic :: Int -> String -> [StaticValue] -> Either String StaticValue
builtinStatic fuel name arguments
    | Just arity <- lookup name probeArities, length arguments == arity = case (name, arguments) of
        ("generatedDemands", [value]) -> do
            demand <- forceStatic fuel value
            case demand of
                StaticDemand label fields -> Right (StaticProbe name label fields)
                _ -> Left "generatedDemands must wrap a statically labelled DemandSupport.demandProbe"
        (_, label:fields) -> do
            text <- staticText fuel label
            Right (StaticProbe name text fields)
        _ -> Left "invalid probe constructor"
    | name == "demandProbe", label:fields <- arguments, length arguments == 5 = do
        text <- staticText fuel label
        Right (StaticDemand text fields)
builtinStatic fuel name arguments = case (name, arguments) of
    ("$", [function, value]) -> forceStatic fuel function >>= \resolved -> applyStatic fuel resolved value
    ("++", [left, right]) -> do
        first <- forceStatic fuel left
        case first of
            StaticList xs -> staticList fuel right >>= checkedList . (xs ++)
            StaticText text -> StaticText . (text ++) <$> staticText fuel right
            _ -> Left "static (++) supports finite lists and labels only"
    (":", [first, rest]) -> staticList fuel rest >>= checkedList . (first:)
    ("concat", [lists]) -> staticList fuel lists >>= concatStatic fuel . map (staticList fuel)
    ("concatMap", [function, list]) -> do
        values <- staticList fuel list
        concatStatic fuel [forceStatic fuel function >>= \resolved -> applyStatic fuel resolved value >>= staticList fuel | value <- values]
    ("map", [function, list]) -> do
        values <- staticList fuel list
        entries <- mapM (\value -> forceStatic fuel function >>= \resolved -> applyStatic fuel resolved value) values
        checkedList entries
    ("filter", [predicate, list]) -> do
        values <- staticList fuel list
        entries <- fmap concat $ forM values $ \value -> do
            function <- forceStatic fuel predicate
            flag <- applyStatic fuel function value >>= staticBool fuel
            Right [value | flag]
        checkedList entries
    (operation, [count, list]) | operation `elem` ["take", "drop"] -> do
        number <- staticNumber fuel count
        values <- staticList fuel list
        let bounded = fromInteger (max 0 (min (toInteger catalogBound + 1) number))
        checkedList ((if operation == "take" then take else drop) bounded values)
    ("replicate", [count, value]) -> do
        number <- staticNumber fuel count
        if number > toInteger catalogBound then Left "static replication exceeds its finite bound"
        else checkedList (replicate (fromInteger (max 0 number)) value)
    ("reverse", [list]) -> staticList fuel list >>= checkedList . reverse
    ("zip", [left, right]) -> do
        xs <- staticList fuel left
        ys <- staticList fuel right
        checkedList (zipWith (\a b -> StaticTuple [a,b]) xs ys)
    ("zipWith", [function, left, right]) -> do
        xs <- staticList fuel left
        ys <- staticList fuel right
        entries <- forM (zip xs ys) $ \(a,b) -> do
            callable <- forceStatic fuel function
            partial <- applyStatic fuel callable a
            applyStatic fuel partial b
        checkedList entries
    (projection, [tuple]) | projection `elem` ["fst", "snd"] -> do
        value <- forceStatic fuel tuple
        case value of
            StaticTuple [a,b] -> forceStatic fuel (if projection == "fst" then a else b)
            _ -> Left "static tuple projection requires a pair"
    ("id", [value]) -> forceStatic fuel value
    ("const", [value, _]) -> forceStatic fuel value
    ("not", [value]) -> StaticBool . not <$> staticBool fuel value
    ("&&", [a,b]) -> do
        first <- staticBool fuel a
        StaticBool <$> if first then staticBool fuel b else Right False
    ("||", [a,b]) -> do
        first <- staticBool fuel a
        StaticBool <$> if first then Right True else staticBool fuel b
    ("show", [value]) -> do
        result <- forceStatic fuel value
        case result of
            StaticNumber number -> Right (StaticText (show number))
            StaticBool flag -> Right (StaticText (show flag))
            _ -> Left "only static numbers/booleans may appear in computed catalog labels"
    _ | name `elem` (staticBuiltins ++ "demandProbe":map fst probeArities), length arguments < 5 ->
            Right (Builtin name arguments)
      | otherwise -> Left ("unsupported static catalog application " ++ name)

concatStatic :: Int -> [Either String [StaticValue]] -> Either String StaticValue
concatStatic _ pieces = StaticList <$> foldM append [] pieces
  where
    append accumulated piece = do
        values <- piece
        combined <- checkedList (accumulated ++ values)
        case combined of StaticList result -> Right result; _ -> Left "invalid static list"

comprehension :: Int -> Environment -> [ExprLStmt GhcPs] -> Either String [StaticValue]
comprehension fuel _ _ | fuel <= 0 = Left "catalog comprehension exceeds its finite expansion bound"
comprehension fuel environment statements = case statements of
    [L _ (LastStmt _ body _ _)] -> Right [Syntax body environment]
    L _ (BindStmt _ pattern' table):rest -> do
        values <- staticList fuel (Syntax table environment)
        combined <- concatStatic fuel [do
            bindings <- matchStatic fuel pattern' value
            comprehension (fuel - 1) (bindings ++ environment) rest | value <- values]
        staticList fuel combined
    L _ (BodyStmt _ condition _ _):rest -> do
        flag <- staticBool fuel (Syntax condition environment)
        if flag then comprehension (fuel - 1) environment rest else Right []
    L _ (LetStmt _ locals):rest -> comprehension (fuel - 1) (extendBindings (localBindings locals) environment) rest
    _ -> Left "only finite generators, static guards and local bindings are supported in catalog comprehensions"

programFields :: StaticValue -> [StaticValue]
programFields (StaticProbe "generatedDemands" _ fields) = take 3 fields
programFields (StaticProbe _ _ fields) = case reverse fields of value:_ -> [value]; [] -> []
programFields _ = []

allFields :: StaticValue -> [StaticValue]
allFields (StaticProbe _ _ fields) = fields
allFields _ = []

fromGenerated :: [String] -> HsModule GhcPs -> RdrName -> Bool
fromGenerated exported ast name = occNameString (rdrNameOcc name) `elem` exported &&
    any (resolves . unLoc) (hsmodImports ast)
  where
    resolves declaration = moduleNameString (unLoc (ideclName declaration)) == "Generated" &&
        importIncludes declaration (occNameString (rdrNameOcc name)) && case name of
            Unqual _ -> ideclQualified declaration == NotQualified
            Qual qualifier _ -> moduleNameString qualifier == maybe "Generated" (moduleNameString . unLoc) (ideclAs declaration)
            _ -> False

fromGeneratedConstructor :: [String] -> [(String, String)] -> HsModule GhcPs -> RdrName -> Bool
fromGeneratedConstructor types exported ast name =
    not locallyDeclared && any (resolves . unLoc) (hsmodImports ast)
  where
    constructor = occNameString (rdrNameOcc name)
    locallyDeclared = case name of
        Unqual _ -> constructor `elem` map snd (declaredConstructors ast)
        _ -> False
    resolves declaration = moduleNameString (unLoc (ideclName declaration)) == "Generated" &&
        any (\identity@(_, selected) -> selected == constructor && included declaration identity) exported &&
        case name of
            Unqual _ -> ideclQualified declaration == NotQualified
            Qual qualifier _ -> moduleNameString qualifier ==
                maybe "Generated" (moduleNameString . unLoc) (ideclAs declaration)
            _ -> False
    included declaration identity = case ideclImportList declaration of
        Nothing -> True
        Just (interpretation, entries) ->
            let selected = any (constructorIncluded types identity . unLoc) (unLoc entries)
            in if interpretation == Exactly then selected else not selected

runtimeConversion :: HsModule GhcPs -> RdrName -> Bool
runtimeConversion ast name = occNameString (rdrNameOcc name) == "reprM" && any (resolves . unLoc) (hsmodImports ast)
  where
    resolves declaration = "Memorist." `isPrefixOf` moduleNameString (unLoc (ideclName declaration)) && case name of
        Unqual _ -> ideclQualified declaration == NotQualified && importIncludes declaration "reprM"
        Qual qualifier _ -> moduleNameString qualifier == maybe
            (moduleNameString (unLoc (ideclName declaration))) (moduleNameString . unLoc) (ideclAs declaration)
        _ -> False

-- Follow only term uses and referenced local definitions, not imports,
-- signatures, or unrelated declarations. Arguments passed through finite tuple
-- tables keep their lexical bindings. This is syntactic reachability, not a
-- proof that arbitrary Haskell uses its argument or takes a particular branch.
reaches :: Bool -> (RdrName -> Bool) -> StaticValue -> Bool
reaches = reachesSyntax False

reachesSyntax :: Bool -> Bool -> (RdrName -> Bool) -> StaticValue -> Bool
reachesSyntax includeConstructors includeTypes target = value 300 []
  where
    value fuel _ _ | fuel <= 0 = False
    value fuel visited (Alias identifier definition)
        | identifier `elem` visited = False
        | otherwise = value (fuel - 1) (identifier:visited) definition
    value fuel visited (Syntax expression environment) = expressionUses fuel visited environment expression
    value fuel visited (Function patterns body locals environment) =
        expressionUses (fuel - 1) visited (shadowPatterns patterns (extendBindings (localBindings locals) environment)) body
    value fuel visited (OpaqueBinding binding environment) = generic (fuel - 1) visited environment binding
    value fuel visited (Builtin _ arguments) = any (value (fuel - 1) visited) arguments
    value fuel visited (StaticTuple entries) = any (value (fuel - 1) visited) entries
    value fuel visited (StaticList entries) = any (value (fuel - 1) visited) entries
    value _ _ _ = False
    variable fuel visited environment name = case lookup (nameKey name) environment of
        Just definition -> value (fuel - 1) visited definition
        Nothing -> target name
    expressionUses fuel visited environment located = case unLoc located of
        HsVar _ name -> (includeConstructors || isVarOcc (rdrNameOcc (unLoc name))) &&
            variable fuel visited environment (unLoc name)
        HsLam _ _ (MG _ alternatives) -> any (matchUses fuel visited environment . unLoc) (unLoc alternatives)
        HsLet _ locals body -> expressionUses (fuel - 1) visited (extendBindings (localBindings locals) environment) body
        HsCase _ subject (MG _ alternatives) -> expressionUses (fuel - 1) visited environment subject ||
            any (matchUses fuel visited environment . unLoc) (unLoc alternatives)
        HsDo _ _ statements -> statementUses fuel visited environment (unLoc statements)
        _ -> generic (fuel - 1) visited environment (unLoc located)
    matchUses fuel visited environment Match { m_pats = patterns, m_grhss = GRHSs { grhssGRHSs = rhss, grhssLocalBinds = locals } } =
        let nested = shadowPatterns patterns (extendBindings (localBindings locals) environment)
        in any (generic (fuel - 1) visited nested . unLoc) rhss
    statementUses _ _ _ [] = False
    statementUses fuel visited environment (statement:rest) = case unLoc statement of
        LetStmt _ locals -> statementUses (fuel - 1) visited (extendBindings (localBindings locals) environment) rest
        BindStmt _ pattern' body -> expressionUses (fuel - 1) visited environment body ||
            statementUses (fuel - 1) visited (shadowPatterns [pattern'] environment) rest
        _ -> generic (fuel - 1) visited environment (unLoc statement) || statementUses (fuel - 1) visited environment rest
    generic :: Data a => Int -> [String] -> Environment -> a -> Bool
    generic fuel _ _ _ | fuel <= 0 = False
    generic fuel visited environment input = case cast input :: Maybe (LHsExpr GhcPs) of
        Just expression -> expressionUses (fuel - 1) visited environment expression
        Nothing -> case cast input :: Maybe (HsType GhcPs) of
            Just (HsTyVar _ _ name) | includeTypes, target (unLoc name) -> True
            _ -> or (gmapQ (generic (fuel - 1) visited environment) input)
    shadowPatterns patterns environment = [(name, Unknown) | name <- patternNames patterns] ++ environment

patternNames :: Data a => a -> [String]
patternNames input = case cast input :: Maybe (Pat GhcPs) of
    Just (VarPat _ name) -> [nameKey (unLoc name)]
    Just (AsPat _ name inner) -> nameKey (unLoc name) : patternNames inner
    _ -> concat (gmapQ patternNames input)

-- Cabal's full language is intentionally not reimplemented. The approved
-- manifests use layout, literal runner fields and one simple common stanza.
-- Unsupported/conditional runner syntax fails closed, including flag overrides
-- that could turn an approved main-is into a different executable.
data Runner = Runner String [FilePath] FilePath

knownManifests :: [(FilePath, [Runner])]
knownManifests =
    [("cost-translate.cabal", [Runner "cost-translate-test" ["test"] "Main.hs"])
    ,("integration/cost-translate-integration.cabal",
        [Runner "cost-translate-integration" ["test-integration", "probes"] "Main.hs"
        ,Runner "runtime-scanl" ["../test"] "RuntimeScanlCandidate.hs"
        ,Runner "runtime-polymorphic-sharing" ["../test"] "RuntimePolymorphicSharingGate.hs"
        ,Runner "runtime-higher-kinded" ["../test"] "RuntimeHigherKindedGate.hs"])]

checkRegistrations :: IO [String]
checkRegistrations = do
    manifestProblems <- fmap concat $ forM knownManifests $ \(file, runners) -> do
        path <- getDataFileName file
        exists <- doesFileExist path
        if not exists then pure [problem file "missing packaged Cabal manifest"]
        else checkManifest file runners <$> readFile path
    root <- getDataFileName "."
    extraProblems <- unregisteredManifests <$> registrationFiles root
    runnerProblems <- fmap concat $ forM runnerFiles $ \file -> do
        path <- getDataFileName file
        exists <- doesFileExist path
        pure [problem file "missing known test runner" | not exists]
    pure (manifestProblems ++ extraProblems ++ runnerProblems)
  where
    runnerFiles = ["test/Main.hs", "integration/test-integration/Main.hs",
        "test/RuntimeScanlCandidate.hs", "test/RuntimePolymorphicSharingGate.hs", "test/RuntimeHigherKindedGate.hs"]

-- An alternate test package in a new subdirectory must not escape the audit.
-- External dependencies, build products, hidden scratch space, and the gate's
-- own declarative negative specimens are not repository test registrations.
registrationFiles :: FilePath -> IO [FilePath]
registrationFiles root = walk ""
  where
    walk relative = do
        names <- sort <$> listDirectory (root </> relative)
        fmap concat $ forM names $ \name -> do
            let file = relative </> name
                full = root </> file
                excluded = "." `isPrefixOf` name || name `elem` ["dist", "dist-newstyle"] ||
                    file `elem` ["hs-pbt", "test/reference-policy"]
            directory <- doesDirectoryExist full
            symbolic <- pathIsSymbolicLink full
            if excluded then pure []
            else if directory && not symbolic then walk file
            else pure [file | takeExtension file == ".cabal"]

unregisteredManifests :: [FilePath] -> [String]
unregisteredManifests files =
    [problem file "unregistered Cabal manifest; semantic tests must use Case.hs discovery"
    | file <- files, file `notElem` map fst knownManifests]

checkManifest :: FilePath -> [Runner] -> String -> [String]
checkManifest file expected source = syntaxProblems ++ suiteProblems ++ commonProblems ++ concatMap checkSuite suites
  where
    at = problem file
    sourceLines = filter (not . null . trim) (map stripComment (lines source))
    syntaxProblems = [at "unsupported Cabal syntax; runner registrations must use plain layout"
                     | any (any (`elem` "{};\t")) sourceLines] ++
        [at "test-suite declarations must be top-level literal registrations" |
            line <- sourceLines, "test-suite" `elem` map lower (words line),
            not (topLevel line && case words line of [kind, _] -> lower kind == "test-suite"; _ -> False)]
    blocks = layoutBlocks sourceLines
    suites = [(name, body) | (header, body) <- blocks, [kind, name] <- [words header], lower kind == "test-suite"]
    names = map fst suites
    wanted = [name | Runner name _ _ <- expected]
    suiteProblems = [at ("unregistered test suite " ++ name ++ "; semantic tests must use Case.hs discovery") | name <- names \\ wanted] ++
        [at ("missing or duplicated known test suite " ++ name) | name <- wanted, length (filter (== name) names) /= 1]
    commonProblems = concat
        [case parseFields body of
            Left reason -> [at ("common stanza: " ++ reason)]
            Right fields ->
                [at "only the settings common stanza is permitted" | words header /= ["common", "settings"]] ++
                [at ("common settings cannot select a runner: " ++ key) | (key, _) <- fields,
                    key `notElem` ["default-language", "ghc-options"]] ++ checkFlags fields
        | (header, body) <- blocks, "common" `isPrefixOf` lower header]
    checkSuite (name, body) = case parseFields body of
        Left reason -> [at (name ++ ": " ++ reason)]
        Right fields -> case [runner | runner@(Runner label _ _) <- expected, label == name] of
            [Runner _ directories entrypoint] ->
                concatMap (exact fields name)
                    [("import", ["settings"]), ("type", ["exitcode-stdio-1.0"]),
                     ("hs-source-dirs", sort directories), ("main-is", [entrypoint])] ++
                [at (name ++ ": unsupported runner field " ++ key) | (key, _) <- fields,
                    key `notElem` ["import", "type", "hs-source-dirs", "main-is", "other-modules",
                        "autogen-modules", "build-depends", "default-language", "default-extensions",
                        "other-extensions", "ghc-options"]] ++ checkFlags fields
            _ -> []
    exact fields name (key, value) =
        [at (name ++ ": " ++ key ++ " must be exactly " ++ unwords value) |
         [sort (tokens actual) | (field, actual) <- fields, field == key] /= [value]]
    checkFlags fields =
        [at ("unapproved runner/compiler option " ++ flag) | ("ghc-options", value) <- fields,
         flag <- words value, flag `notElem` ["-Wall", "-Werror", "-Wcompat", "-threaded", "-rtsopts", "-O0", "-O1", "-O2"]] ++
        [at ("unapproved runner language extension " ++ extension) |
         (key, value) <- fields, key `elem` ["default-extensions", "other-extensions"],
         extension <- tokens value, extension `notElem` permittedExtensions]
    tokens = words . map (\c -> if c == ',' then ' ' else c)

layoutBlocks :: [String] -> [(String, [String])]
layoutBlocks [] = []
layoutBlocks (header:rest) =
    let (body, remaining) = break topLevel rest
    in (header, body) : layoutBlocks remaining

topLevel :: String -> Bool
topLevel (first:_) = not (isSpace first)
topLevel [] = False

parseFields :: [String] -> Either String [(String, String)]
parseFields [] = Right []
parseFields sourceLines = go sourceLines
  where
    indentation = minimum (map (length . takeWhile isSpace) sourceLines)
    go [] = Right []
    go (line:rest)
        | length (takeWhile isSpace line) /= indentation = Left "unsupported nested/conditional runner fields"
        | otherwise = case break (== ':') (trim line) of
            (key, _:value) | not (null key), all (\c -> isAlphaNum c || c == '-') key -> do
                let (continuation, remaining) = span ((> indentation) . length . takeWhile isSpace) rest
                if any suspicious continuation then Left "unsupported nested/conditional runner fields"
                else ((lower key, unwords (trim value : map trim continuation)) :) <$> go remaining
            _ -> Left "unsupported conditional or non-field runner syntax"
    suspicious line = ':' `elem` line || case words (lower (trim line)) of
        first:_ -> first `elem` ["if", "else", "elif", "test-suite", "common"]
        [] -> False

stripComment :: String -> String
stripComment [] = []
stripComment ('-':'-':_) = []
stripComment (c:rest) = c : stripComment rest

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

lower :: String -> String
lower = map toLower

checkReferencePolicyTests :: IO ()
checkReferencePolicyTests = do
    flags <- runGhc (Just libdir) getSessionDynFlags
    mapM_ (expectSource flags) sourceSpecimens
    mapM_ expectInventory inventorySpecimens
    mapM_ expectManifest manifestSpecimens
    mapM_ expectRegistrations
        [("registration-valid", []), ("registration-nested", ["unregistered Cabal manifest"])]
    let exampleFiles = [source | (_, source) <- manualExamples]
        exampleCases = [(label, [source]) | (label, source) <- manualExamples]
        exampleInstructions = unlines $ ["| Source | Selector |", "| --- | --- |"] ++
            ["| `" ++ takeFileName source ++ "` | `" ++ label ++ "` |" | (label, source) <- manualExamples]
    expect "manual-examples/valid" [] (manualExampleProblems exampleFiles exampleCases exampleInstructions)
    expect "manual-examples/missing-source" ["missing manual example"]
        (manualExampleProblems [] exampleCases exampleInstructions)
    expect "manual-examples/missing-case" ["registered semantic source"]
        (manualExampleProblems exampleFiles [] exampleInstructions)
    expect "manual-examples/copied-source" ["registered semantic source"]
        (manualExampleProblems exampleFiles
            [(label, ["test/cases/copied/Example.hs"]) | (label, _) <- manualExamples]
            exampleInstructions)
    expect "manual-examples/missing-instructions" ["missing source/selector row"]
        (manualExampleProblems exampleFiles exampleCases "")
    expect "manual-examples/missing-filename" ["missing source/selector row"]
        (manualExampleProblems exampleFiles exampleCases (unlines [label | (label, _) <- manualExamples]))
    expect "manual-examples/missing-selector" ["missing source/selector row"]
        (manualExampleProblems exampleFiles exampleCases (unlines (map takeFileName exampleFiles)))
    expect "manual-examples/split-instructions" ["missing source/selector row"]
        (manualExampleProblems exampleFiles exampleCases
            (unlines (concat [[takeFileName source, label] | (label, source) <- manualExamples])))
    putStrLn "PASS: reference-policy / parsed static catalogs, isolated references, fail-closed inventory and test registrations"
  where
    expectSource flags (role, specimen, fragments) = do
        let file = "test/reference-policy" </> specimen
        ownReference <- if role == CaseFile
            then parseExisting flags ReferenceFile (takeDirectory file </> "Reference.hs") else pure Nothing
        reference <- case ownReference of
            Just parsed -> pure parsed
            Nothing -> parseFile flags ReferenceFile "test/reference-policy/valid/Reference.hs"
        parsed <- parseFile flags role file
        let generated = either (const emptyGeneratedNames) (generatedNames . snd) reference
            problems = either id (\(fileFlags, ast) -> checkModule fileFlags role generated file ast) parsed
        expect file fragments problems
    expectInventory (specimen, fragments) = do
        path <- getDataFileName ("test/reference-policy" </> specimen)
        inventory <- readInventory path
        let (_, problems) = inventoryCases False inventory
        expect specimen fragments problems
    expectManifest (specimen, fragments) = do
        let file = "test/reference-policy" </> specimen
        path <- getDataFileName file
        source <- readFile path
        expect file fragments (checkManifest file [Runner "cost-translate-test" ["test"] "Main.hs"] source)
    expectRegistrations (specimen, fragments) = do
        path <- getDataFileName ("test/reference-policy" </> specimen)
        files <- registrationFiles path
        expect specimen fragments (unregisteredManifests files)
    expect file fragments problems = unless
        (if null fragments then null problems else all (\fragment -> any (fragment `isInfixOf`) problems) fragments) $
        fail (file ++ ": reference-policy selftest expected " ++ show fragments ++ ", got\n" ++ unlines problems)

sourceSpecimens :: [(FileRole, FilePath, [String])]
sourceSpecimens =
    [(CaseFile, "valid/Case.hs", []), (CaseFile, "valid/SelectedCase.hs", [])
    ,(CaseFile, "valid/ConcatCase.hs", [])
    ,(CaseFile, "valid/CombinatorsCase.hs", [])
    ,(CaseFile, "valid/DemandCase.hs", [])
    ,(CaseFile, "public-constructor/Case.hs", [])
    ,(CaseFile, "constructor-only/ReachableCase.hs", [])
    ,(CaseFile, "constructor-provenance/ExplicitCase.hs", [])
    ,(CaseFile, "constructor-provenance/RecordCase.hs", [])
    ] ++
    [(CaseFile, "constructor-provenance" </> name ++ "Case.hs", ["costed callback has no reachable Generated"])
        | name <- ["Runtime", "Type", "Pattern", "Unreachable", "Local", "Shadow", "Private",
                   "Abstract", "Ambiguous", "Qualified", "Unqualified", "HiddenImport", "TypeImport",
                   "SameNameImport", "Unrelated", "AbstractRuntime", "PrivateRecord", "RecordImport"]] ++
    [(ReferenceFile, "valid/Reference.hs", [])
    ,(CaseFile, "custom-main/Case.hs", ["must declare module Case", "name main"])
    ,(CaseFile, "trial-knobs/Case.hs", ["name maxSuccess", "name quickCheckWith"])
    ,(CaseFile, "property/Case.hs", ["name Property"])
    ,(CaseFile, "io/Case.hs", ["name IO"])
    ,(CaseFile, "implicit-io/Case.hs", ["name print"])
    ,(CaseFile, "quickcheck-import/Case.hs", ["forbidden import Test.QuickCheck"])
    ,(CaseFile, "dynamic-catalog/Case.hs", ["static signature", "argument-free"])
    ,(CaseFile, "altered-catalog/Case.hs", ["successful costed computation"])
    ,(CaseFile, "dynamic-label/Case.hs", ["unsupported static probe catalog"])
    ,(CaseFile, "duplicate-label/Case.hs", ["labels must be unique"])
    ,(CaseFile, "shadow-support/Case.hs", ["cannot redefine ReferenceSupport"])
    ,(CaseFile, "local-shadow/Case.hs", ["cannot redefine ReferenceSupport"])
    ,(CaseFile, "hidden-support/Case.hs", ["without qualification, aliases, or hiding"])
    ,(CaseFile, "missing-constructor-import/Case.hs", ["must come from ReferenceSupport"])
    ,(CaseFile, "snapshot-only/Case.hs", ["costed computation"])
    ,(CaseFile, "missing-import/Case.hs", ["missing required import Generated"])
    ,(CaseFile, "source-only/Case.hs", ["costed callback has no reachable Generated"])
    ,(CaseFile, "instance-only/Case.hs", ["Generated () is forbidden"])
    ,(CaseFile, "disconnected/Case.hs", ["costed callback has no reachable Generated"])
    ,(CaseFile, "partially-disconnected/Case.hs", ["costed callback has no reachable Generated"])
    ,(CaseFile, "helper-source-only/Case.hs", ["costed callback has no reachable Generated"])
    ,(CaseFile, "constructor-only/Case.hs", ["costed callback has no reachable Generated"])
    ,(CaseFile, "shadow-generated/Case.hs", ["costed callback has no reachable Generated"])
    ,(CaseFile, "unbounded-catalog/Case.hs", ["unsupported static probe catalog"])
    ,(CaseFile, "failure-only/Case.hs", ["successful costed computation"])
    ,(ReferenceFile, "wrong-module/Reference.hs", ["must declare module Generated"])
    ,(ReferenceFile, "self-import/Reference.hs", ["forbidden import Generated"])
    ,(ReferenceFile, "source-value/Reference.hs", ["source value calls are forbidden"])
    ,(ReferenceFile, "unqualified-source/Reference.hs", ["Example imports must be qualified"])
    ,(ReferenceFile, "system-import/Reference.hs", ["forbidden import System.Process"])
    ,(ReferenceFile, "translator-import/Reference.hs", ["forbidden import Cost.Translate"])
    ,(ReferenceFile, "local-import/Reference.hs", ["forbidden import Hidden"])
    ,(ReferenceFile, "fake-base-import/Reference.hs", ["forbidden import Data.Hidden"])
    ,(ReferenceFile, "quickcheck-runner-import/Reference.hs", ["forbidden import Test.QuickCheck", "name quickCheck"])
    ,(ReferenceFile, "unsafe/Reference.hs", ["name unsafeCoerce"])
    ,(ReferenceFile, "cpp/Reference.hs", ["LANGUAGE extension CPP", "CPP directives"])
    ,(ReferenceFile, "template-haskell/Reference.hs", ["LANGUAGE extension TemplateHaskell"])
    ,(ReferenceFile, "plugin/Reference.hs", ["no compiler hooks"])
    ,(ReferenceFile, "foreign/Reference.hs", ["foreign declarations"])
    ,(ReferenceFile, "parse-error/Reference.hs", ["cannot parse Haskell input"])]

inventorySpecimens :: [(FilePath, [String])]
inventorySpecimens =
    [("inventory-valid", [])
    ,("inventory-missing-reference", ["Reference.hs: reference policy: missing"])
    ,("inventory-missing-case", ["Case.hs: reference policy: missing"])
    ,("inventory-missing-source", ["Example.hs: reference policy: missing"])
    ,("inventory-source-only", ["Case.hs: reference policy: missing", "Reference.hs: reference policy: missing"])
    ,("inventory-main", ["custom Main.hs is forbidden"])
    ,("inventory-regression", ["Regression.hs is not permitted"])
    ,("inventory-extra-module", ["unaccounted Haskell source"])
    ,("inventory-diagnostic", ["unregistered diagnostic"])
    ,("inventory-infrastructure", ["unregistered diagnostic"])
    ,("inventory-nested", ["unaccounted nested fixture directory"])]

manifestSpecimens :: [(FilePath, [String])]
manifestSpecimens =
    [("manifest-valid/fixture.cabal", [])
    ,("manifest-extra-suite/fixture.cabal", ["unregistered test suite bypass"])
    ,("manifest-custom-main/fixture.cabal", ["main-is must be exactly Main.hs"])
    ,("manifest-main-flag/fixture.cabal", ["unapproved runner/compiler option -main-is"])
    ,("manifest-conditional/fixture.cabal", ["unsupported conditional"])
    ,("manifest-common-main/fixture.cabal", ["common settings cannot select a runner"])]
