module Cost.Translate.Frontend.Prepare (generate) where

import Control.Monad.IO.Class (liftIO)
import Cost.AcceptedSource (AcceptedSource, acceptedSummary)
import Cost.Translate.Callable (expressions)
import Cost.Translate.Datatypes (checkConversionExports)
import Cost.Translate.Diagnostic (Diagnostic (..))
import Cost.Translate.Frontend.Expression.Evidence
import Cost.Translate.Frontend.Gates
import Cost.Translate.Frontend.Module (emitModule)
import Cost.Translate.Frontend.Prune (pruneLocals)
import Cost.Translate.Frontend.Strict (strictWhnfBindings)
import Cost.Translate.Frontend.Syntax (collect)
import Cost.Translate.Pretty (prettyModule)
import Cost.Translate.Typed qualified as Typed
import Cost.Translate.Validation (validateComparisons)
import Data.Bifunctor (first)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC
import GHC.Core.Type (splitTyConApp_maybe)
import GHC.Driver.Session (xopt)
import GHC.LanguageExtensions.Type qualified as Extension
import GHC.Paths (libdir)
import GHC.Types.Name (getOccString)
import GHC.Types.Var (isId, varName)
import GHC.Unit.Module.ModGuts (mg_binds)
import System.FilePath (takeDirectory)

generate ::
    FilePath ->
    String ->
    IO (Either Diagnostic (String, AcceptedSource), String)
generate file targetModule = runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    _ <-
        setSessionDynFlags
            flags
                { backend = noBackend
                , ghcLink = NoLink
                , importPaths = [takeDirectory file]
                }
    target <- guessTarget file Nothing Nothing
    setTargets [target]
    graph <- depanal [] False
    case mgModSummaries graph of
        [summary] -> do
            (accepted, receipt) <- liftIO (acceptedSummary summary)
            (result, logs) <- generateChecked file targetModule flags accepted
            pure (fmap (,receipt) result, logs)
        _ -> pure (sourceFailure file ": expected one source module")

generateChecked ::
    FilePath ->
    String ->
    DynFlags ->
    ModSummary ->
    Ghc (Either Diagnostic String, String)
generateChecked file target flags summary = do
    checked <- parseModule summary >>= typecheckModule
    desugared <- desugarModule checked
    session <- getSession
    evidence <- case tm_renamed_source checked of
        Just (group, _, _, _, _) -> do
            let source = expressions group
                typed = collect (tm_typechecked_source checked)
            cores <- liftIO (Typed.expressionCores session checked source typed)
            pure (checkedExpressions checked source typed cores)
        Nothing -> pure (ExpressionEvidence Map.empty Map.empty)
    strictValues <- liftIO (strictWhnfBindings session checked)
    pure $
        renderChecked
            file
            target
            flags
            summary
            checked
            desugared
            evidence
            strictValues

renderChecked ::
    FilePath ->
    String ->
    DynFlags ->
    ModSummary ->
    TypecheckedModule ->
    DesugaredModule ->
    ExpressionEvidence ->
    Set.Set Name ->
    (Either Diagnostic String, String)
renderChecked file target flags summary checked desugared evidence strict =
    case tm_renamed_source checked of
        Just (group, imports, _, _, _) ->
            let info = tm_checked_module_info checked
                datatypes = [t | ATyCon t <- modInfoTyThings info]
                emit =
                    emitModule
                        strict
                        target
                        (moduleNameString (moduleName (ms_mod summary)))
                        (xopt Extension.MagicHash (ms_hspp_opts summary))
                        datatypes
                        (bindingTypes checked)
                        (literalTypes checked)
                        evidence
                        (Typed.functionTypes checked)
                (original, logs) = emit group
                result = checkedPrunedEmission strict group emit original
                validation = do
                    checkSourceExtensions file summary
                    mapM_ checkImport imports
                    mapM_ checkDeclaration sourceDeclarations
                    checkConversionExports
                        (modInfoExports info)
                        datatypes
                    validateComparisons (mg_binds (coreModule desugared))
                sourceDeclarations = hsmodDecls (unLoc (parsedSource checked))
                render (ast, sections, extensions) =
                    prettyModule flags extensions ast sections
             in ( first UnsupportedSource (validation >> result) >>= render
                , logs
                )
        Nothing -> sourceFailure file ": GHC did not retain renamed source"

sourceFailure :: FilePath -> String -> (Either Diagnostic a, String)
sourceFailure file message = (Left (UnsupportedSource (file ++ message)), "")

checkedPrunedEmission ::
    Set.Set Name ->
    HsGroup GhcRn ->
    (HsGroup GhcRn -> (Either String a, String)) ->
    Either String a ->
    Either String a
checkedPrunedEmission strictValues group emit original =
    let pruned = pruneLocals strictValues group
        changed =
            length (collect pruned :: [LHsBind GhcRn])
                /= length (collect group :: [LHsBind GhcRn])
     in if changed then original >> fst (emit pruned) else original

bindingTypes :: TypecheckedModule -> Map.Map Name Type
bindingTypes checked =
    Map.fromList
        [ (varName i, idType i)
        | AnId i <- modInfoTyThings (tm_checked_module_info checked)
        ]
        `Map.union` Map.fromList
            [ (varName i, idType i)
            | i <- collect (tm_typechecked_source checked) :: [Id]
            , isId i
            ]

literalTypes :: TypecheckedModule -> Map.Map Typed.SourceKey String
literalTypes checked =
    Map.mapMaybe unique $
        Map.fromListWith
            (++)
            [ (Typed.sourceKey (getLocA expression), [name])
            | expression@(L _ (HsOverLit _ literal)) <-
                collect (tm_typechecked_source checked) :: [LHsExpr GhcTc]
            , Just (tc, []) <- [splitTyConApp_maybe (overLitType literal)]
            , let name = getOccString tc
            , name `elem` ["Int", "Integer", "Float", "Double"]
            ]
    where
        unique [name] = Just name
        unique _ = Nothing
