module GeneratedConstruction (checkGeneratedConstruction, checkGeneratedConstructionSpecimens) where

import Control.Exception (IOException, displayException, try)
import Control.Monad (forM, forM_, unless)
import Data.Data (Data, cast, gmapQ)
import Data.Foldable (toList)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, isJust, maybeToList)
import GHC
    ( AmbiguousFieldOcc(..), FieldOcc(..), GhcRn, GhcTc, HsExpr(..), HsFieldBind(..)
    , HsMatchContext(..), Id, LIdP, LHsRecUpdFields(..), Match(..), MatchGroup(..), Name, Pat(..), SrcSpan, Type
    , GhcLink(NoLink), LoadHowMuch(LoadAllTargets), SuccessFlag(..), TyThing(..)
    , backend, findModule, getLocA, getLogger, getModuleInfo
    , getSessionDynFlags, guessTarget, importPaths, mgModSummaries, mkModuleName
    , load, lookupName, ml_hs_file, modInfoExports, ms_location, noBackend, parseDynamicFlags, parseModule, depanal
    , runGhc, setSessionDynFlags, setTargets, tm_renamed_source, tm_typechecked_source, typecheckModule
    , unLoc, ghcLink
    )
import GHC.Core.ConLike (ConLike(..), conLikeName)
import GHC.Hs.Expr (HsRecUpdParent(..), HsThingRn(..), HsWrap(..), XXExprGhcTc(..))
import GHC.Paths (libdir)
import GHC.Tc.Types.Evidence (HsWrapper, TcEvBinds)
import GHC.Types.GREInfo (conLikeName_Name)
import GHC.Types.Name (getName, getOccString, nameModule_maybe)
import GHC.Types.Name.Reader (greName)
import GHC.Types.SrcLoc (noLoc)
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import Paths_cost_translate (getDataFileName)
import System.FilePath ((</>), takeDirectory)

-- Resolve actual runtime definitions, not an import's spelling or qualifier.
-- The renamed tree retains references in unused local/top-level helpers.
checkGeneratedConstruction :: FilePath -> IO ()
checkGeneratedConstruction output = do
    problems <- runGhc (Just libdir) $ do
        flags <- getSessionDynFlags
        logger <- getLogger
        (configured, leftovers, _) <- parseDynamicFlags logger flags
            (map noLoc ["-package", "MemoristANT", "-package", "QuickCheck"])
        unless (null leftovers) (fail "cannot configure generated-construction runtime packages")
        _ <- setSessionDynFlags configured
            { backend = noBackend, ghcLink = NoLink, importPaths = [takeDirectory output] }
        forbidden <- concat <$> forM reasoningOperations (\(definingModule, occurrences) -> do
            actualModule <- findModule (mkModuleName definingModule) Nothing
            info <- getModuleInfo actualModule >>= maybe
                (fail ("cannot inspect reasoning operations in " ++ definingModule)) pure
            let definedHere = [name | name <- modInfoExports info, nameModule_maybe name == Just actualModule]
                required = fromMaybe (map getOccString definedHere) occurrences
            forM required $ \occurrence -> case
                    [name | name <- definedHere, getOccString name == occurrence] of
                [name] -> pure (name, definingModule ++ "." ++ occurrence)
                _ -> fail ("cannot resolve reasoning operation " ++ definingModule ++ "." ++ occurrence))
        coreModule <- findModule (mkModuleName "Memorist.Base.Core") Nothing
        coreInfo <- getModuleInfo coreModule >>= maybe (fail "cannot inspect Annotated constructor") pure
        annotatedNames <- mapM lookupName
            [ name | name <- modInfoExports coreInfo, nameModule_maybe name == Just coreModule
            , getOccString name == "Annotated" ]
        -- The type and data constructor share an occurrence string, not a Name.
        annotatedConstructor <- case
                [getName constructor | Just (AConLike (RealDataCon constructor)) <- annotatedNames] of
            [name] -> pure name
            _ -> fail "cannot resolve Annotated data constructor"
        target <- guessTarget output Nothing Nothing
        setTargets [target]
        graph <- depanal [] False
        -- Load home imports as GHC normally would (e.g. source conversion types
        -- or a re-export), but inspect only this generated artifact's tree.
        loaded <- load LoadAllTargets
        case loaded of
            Failed -> fail (output ++ ": cannot load generated module for name resolution")
            Succeeded -> pure ()
        checked <- case [summary | summary <- mgModSummaries graph,
                ml_hs_file (ms_location summary) == Just output] of
            [summary] -> parseModule summary >>= typecheckModule
            _ -> fail (output ++ ": expected one generated module")
        case tm_renamed_source checked of
            Just (group, _, _, _, _) -> do
                let references =
                        [(getLocA reference, unLoc reference) | reference <- collectAst group :: [LIdP GhcRn]] ++
                        [(getLocA label, name) | FieldOcc name label <- collectAst group :: [FieldOcc GhcRn]] ++
                        [(getLocA label, name) | Unambiguous name label <- collectAst group :: [AmbiguousFieldOcc GhcRn]] ++
                        ambiguousUpdateReferences (tm_typechecked_source checked)
                    violations =
                        [(location, operation) | (location, name) <- references, Just operation <- [lookup name forbidden]] ++
                        [(getLocA constructor, "Memorist.Base.Core.Annotated")
                        | ConPat _ constructor _ <- collectAst group :: [Pat GhcRn]
                        , unLoc constructor == annotatedConstructor]
                pure
                    [ showSDocUnsafe (ppr location) ++ ": reasoning-only operation " ++
                      operation ++ " is forbidden in generated code (" ++ output ++ ")"
                    | (location, operation) <- violations
                    ]
            Nothing -> fail (output ++ ": GHC did not retain renamed source")
    unless (null problems) (fail (unlines problems))

-- GHC 9.10 expands updates to let/case before retaining the typechecked tree.
-- Its selected constructor identifies the right parent, not merely a candidate
-- with the same field spelling. Parent labels are ordered like update fields.
ambiguousUpdateReferences :: Data a => a -> [(SrcSpan, Name)]
ambiguousUpdateReferences checked =
    [ (getLocA label, greName field)
    | ExpandedThingTc (OrigExpr RecordUpd { rupd_flds = RegularRecUpdFields parents fields }) expanded <- collectAst checked
    , parent <- toList parents
    , any ((`elem` updateConstructors expanded) . conLikeName_Name) (nonDetEltsUniqSet (rnRecUpdCons parent))
    , (binding, field) <- zip fields (toList (rnRecUpdLabels parent))
    , Ambiguous _ label <- [unLoc (hfbLHS (unLoc binding))]
    ]

updateConstructors :: HsExpr GhcTc -> [Name]
updateConstructors (HsLet _ _ body) = updateConstructors (unLoc body)
updateConstructors (XExpr (WrapExpr (HsWrap _ expression))) = updateConstructors expression
updateConstructors (HsCase RecUpd _ matches) =
    [ conLikeName (unLoc constructor)
    | match <- unLoc (mg_alts matches)
    , ConPat _ constructor _ <- map unLoc (m_pats (unLoc match))
    ]
updateConstructors _ = []

-- Nothing selects the whole cost-reasoning module, including its property APIs.
-- Thunk operations and legitimate demand-conversion APIs remain legal.
reasoningOperations :: [(String, Maybe [String])]
reasoningOperations =
    [("Memorist.Base.Core", Just
        ["eval", "eval1", "value", "annot", "costAnnot", "usageAnnot", "runM", "runMMBT", "runMBT", "runAnnotT"])
    ,("Memorist.Base.ANT", Just ["runMT"])
    ,("Memorist.Infer.Infer", Nothing)]

collectAst :: (Data a, Data b) => b -> [a]
collectAst value
    -- Typechecked syntax contains semantic graphs, which are not source uses.
    | isJust (cast value :: Maybe Id) = []
    | isJust (cast value :: Maybe Type) = []
    | isJust (cast value :: Maybe ConLike) = []
    | isJust (cast value :: Maybe HsWrapper) = []
    | isJust (cast value :: Maybe TcEvBinds) = []
    | otherwise = maybeToList (cast value) ++ concat (gmapQ collectAst value)

checkGeneratedConstructionSpecimens :: IO ()
checkGeneratedConstructionSpecimens = do
    rejects "QualifiedAlias.hs" [(4, "Memorist.Base.Core.value")]
    rejects "Unqualified.hs" [(7, "Memorist.Base.Core.eval1")]
    rejects "LetWrapper.hs" [(6, "Memorist.Base.Core.value")]
    rejects "UnusedHelper.hs" [(6, "Memorist.Base.Core.eval1")]
    rejects "Reexport.hs" [(6, "Memorist.Base.Core.value")]
    rejects "BoundaryReferences.hs"
        [(7, "Memorist.Base.Core.eval"), (10, "Memorist.Base.Core.eval1")
        ,(13, "Memorist.Base.Core.value"), (16, "Memorist.Base.Core.runM")
        ,(19, "Memorist.Base.Core.runMMBT"), (20, "Memorist.Base.Core.runMBT")
        ,(21, "Memorist.Base.Core.runAnnotT"), (22, "Memorist.Base.ANT.runMT")]
    rejects "ReasoningPublication.hs" [(8, "Memorist.Base.Core.value"), (8, "Memorist.Base.Core.eval1")]
    rejects "ReasoningAPIs.hs"
        [(6, "Memorist.Infer.Infer.infcost"), (9, "Memorist.Infer.Infer.infcost'")
        ,(12, "Memorist.Infer.Infer.calcCost"), (15, "Memorist.Infer.Infer.correct")
        ,(18, "Memorist.Base.Core.annot"), (21, "Memorist.Base.Core.costAnnot")
        ,(24, "Memorist.Base.Core.usageAnnot")]
    rejects "RecordUpdate.hs" [(6, "Memorist.Base.Core.value")]
    rejects "AmbiguousRecordUpdate.hs" [(10, "Memorist.Base.Core.value")]
    rejects "AnnotatedPattern.hs" [(6, "Memorist.Base.Core.Annotated"), (11, "Memorist.Base.Core.Annotated")]
    rejects "AnnotatedRecordPatterns.hs"
        [(7, "Memorist.Base.Core.Annotated"), (10, "Memorist.Base.Core.Annotated")
        ,(13, "Memorist.Base.Core.Annotated"), (16, "Memorist.Base.Core.Annotated")]
    mapM_ accepts
        ["ImportAliasControl.hs", "LocalShadowing.hs", "Construction.hs", "RecordUpdateControl.hs", "AnnotatedPatternControl.hs"]
    putStrLn "PASS: generated-construction / resolved reasoning references, record updates and Annotated patterns rejected, including unused helpers / user identities, type mentions, construction and thunk conversions accepted"
  where
    specimen name = getDataFileName ("test" </> "generated-construction" </> name)
    accepts name = specimen name >>= checkGeneratedConstruction
    rejects name expected = do
        output <- specimen name
        result <- try (checkGeneratedConstruction output) :: IO (Either IOException ())
        case result of
            Left problem -> forM_ expected $ \(line, operation) ->
                unless (any (\diagnostic -> all (`isInfixOf` diagnostic)
                        [output ++ ":" ++ show (line :: Int) ++ ":", "reasoning-only operation " ++ operation,
                         "is forbidden in generated code"]) (lines (displayException problem))) $
                    fail (output ++ ": expected forbidden " ++ operation ++ ", got " ++ displayException problem)
            Right () -> fail (output ++ ": expected forbidden reasoning operation, got Right ()")
