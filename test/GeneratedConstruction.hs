module GeneratedConstruction (checkGeneratedConstruction, checkGeneratedConstructionSpecimens) where

import Control.Exception (IOException, displayException, try)
import Control.Monad (forM, forM_, unless)
import Data.Data (Data, cast, gmapQ)
import Data.Foldable (toList)
import Data.List (isInfixOf)
import Data.Maybe (isJust, maybeToList)
import ConstructionAst (pretty)
import ConstructionEvidence (checkEvidence, methodNames)
import ConstructionDemand (demandSupport, homeDemandTypes)
import ConstructionRoles (supportBindings, supportCoreNames)
import ConstructionRuntime (runtime, runtimeOwns, roleOf, roleNames, Role(..), describeName, standardNames)
import ConstructionSyntax (checkCompilerOptions, checkParsedSyntax, checkSyntax)
import ConstructionViews (viewSupport)
import GHC
    ( AmbiguousFieldOcc(..), GhcTc, HsExpr(..), HsFieldBind(..)
    , HsMatchContext(..), Id, LHsRecUpdFields(..), Match(..), MatchGroup(..), Name, Pat(..), SrcSpan, Type
    , GhcLink(NoLink), LoadHowMuch(LoadAllTargets), SuccessFlag(..)
    , backend, getLocA, getLogger
    , getSessionDynFlags, guessTarget, importPaths, mgModSummaries, ms_mod
    , load, noBackend, parseDynamicFlags, parseModule, depanal
    , runGhc, setSessionDynFlags, setTargets, tm_renamed_source, tm_typechecked_source, typecheckModule
    , tm_internals_
    , unLoc, ghcLink
    )
import GHC.Core.ConLike (ConLike, conLikeName)
import GHC.Core.TyCon (isAlgTyCon, isClassTyCon, tyConDataCons)
import GHC.Hs.Expr (HsRecUpdParent(..), HsThingRn(..), HsWrap(..), XXExprGhcTc(..))
import GHC.Paths (libdir)
import GHC.Tc.Types.Evidence (HsWrapper, TcEvBinds)
import GHC.Tc.Types (tcg_tcs)
import GHC.Types.Name (getName, isDataConName)
import GHC.Types.GREInfo (conLikeName_Name)
import GHC.Types.Name.Reader (greName)
import GHC.Types.SrcLoc (noLoc)
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import Paths_cost_translate (getDataFileName)
import System.FilePath ((</>), takeDirectory)
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

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
        rt <- runtime
        methods <- methodNames rt
        target <- guessTarget output Nothing Nothing
        setTargets [target]
        graph <- depanal [] False
        let summaries = mgModSummaries graph
            homes = map ms_mod summaries
            optionProblems = concatMap checkCompilerOptions summaries
        unless (null optionProblems) (fail (unlines optionProblems))
        parsedModules <- mapM parseModule summaries
        let parsedProblems = concatMap checkParsedSyntax parsedModules
        unless (null parsedProblems) (fail (unlines parsedProblems))
        -- A home import is not an opaque dependency certificate. Inspect every
        -- body in the complete loaded home graph, even an unused imported wrapper.
        loaded <- load LoadAllTargets
        case loaded of
            Failed -> fail (output ++ ": cannot load generated module for name resolution")
            Succeeded -> pure ()
        checkedModules <- mapM typecheckModule parsedModules
        let constructors = concatMap (roleNames rt) [Representation, DemandData] ++
                [name | (_, name) <- standardNames rt, isDataConName name] ++
                [getName constructor | checked <- checkedModules, tycon <- tcg_tcs (fst (tm_internals_ checked)),
                    isAlgTyCon tycon, not (isClassTyCon tycon), constructor <- tyConDataCons tycon]
        (demand, restricted) <- demandSupport rt constructors checkedModules
        (views, viewNames, viewCalls) <- viewSupport rt homes checkedModules
        fmap concat $ forM checkedModules $ \checked -> do
          case tm_renamed_source checked of
            Just (group, _, _, _, _) -> do
                let support = supportBindings rt constructors group ++ demand ++ views
                    (syntaxProblems, sites) = checkSyntax rt homes methods (restricted ++ viewNames) support viewCalls group
                    updates = [pretty location ++ ": unapproved runtime operation " ++ describeName name ++
                        " is forbidden in generated code" | (location, name) <- ambiguousUpdateReferences (tm_typechecked_source checked),
                        runtimeOwns rt name, roleOf rt name `notElem` [Just Primitive, Just Representation, Just DemandData]]
                evidenceProblems <- checkEvidence rt homes (homeDemandTypes rt checkedModules) (supportCoreNames support checked) checked sites
                pure (syntaxProblems ++ updates ++ evidenceProblems)
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

-- The construction interface is closed: a newly exported value is not permitted
-- merely because the old evaluator blacklist did not anticipate it.
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
    rejects "ManualNaming.hs" [(6, "Memorist.Base.Thunk.nnext")]
    rejects "PayloadComputation.hs" [(6, "Memorist.Base.Thunk.tval")]
    rejects "AnnotationConstruction.hs" [(6, "Memorist.Base.Core.annotate")]
    rejects "AnnotatedConstruction.hs" [(6, "Memorist.Base.Core.Annotated")]
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
    rejects "InstancePayload.hs" [(10, "Memorist.Base.Thunk.tval")]
    rejectsBecause "PureCarrier.hs" ["PureCarrier.hs:7:", "PureCarrier.hs:10:", "unapproved selected method", "GHC.Internal.Base.pure", "audited Memorist instance"]
    rejectsBecause "BindCarrier.hs" ["unapproved selected method", "GHC.Internal.Base.>>=", "audited Memorist instance"]
    rejectsBecause "ApplicativeCarrier.hs" ["unapproved selected method", "GHC.Internal.Base.<*>", "audited Memorist instance"]
    rejectsBecause "ProtectedEq.hs" ["ProtectedEq.hs:6:", "ProtectedEq.hs:10:", "unapproved selected method", "protected thunk/annotation/accounting operand"]
    rejectsBecause "PatternPayload.hs" ["unsupported executable form: pattern synonym"]
    rejectsBecause "UnknownExternal.hs" ["unapproved external operation GHC.Internal.Data.Typeable.Internal.someTypeRep"]
    rejectsBecause "UnknownRuntime.hs" ["unapproved runtime operation Memorist.Base.Extension.composeTM"]
    rejectsBecause "ImportedWrapper.hs" ["HiddenConstruction.hs:6:", "unapproved runtime operation Memorist.Base.Thunk.tval"]
    rejectsBecause "DemandComputation.hs" ["unapproved external operation DemandControl.shrinkView"]
    rejectsBecause "AlteredDemandView.hs" ["unapproved runtime operation Memorist.Base.Thunk.tval"]
    rejectsBecause "ViewAlteredMetadata.hs" ["unapproved runtime operation Memorist.Base.Thunk.Th"]
    rejectsBecause "ViewArbitraryConverter.hs" ["unchecked structural view use/escape"]
    rejectsBecause "ViewDeadConverter.hs" ["unapproved runtime operation Memorist.Base.Core.eval1"]
    rejectsBecause "ViewEscape.hs" ["unchecked structural view use/escape"]
    rejectsBecause "ViewCrossInput.hs" ["unchecked structural view use/escape"]
    rejectsBecause "ViewRawPattern.hs" ["unapproved runtime operation Memorist.Base.Thunk.Th"]
    mapM_ (\name -> rejectsBecause name ["unchecked structural view use/escape"])
        ["SchemaIndependentFields.hs", "SchemaChangedConstructor.hs", "SchemaCallback.hs", "SchemaEscape.hs",
         "SchemaTypeOnly.hs", "SchemaOpaque.hs"]
    rejectsBecause "SchemaDeadHelper.hs" ["unapproved runtime operation Memorist.Base.Thunk.tval"]
    rejectsBecause "DeferredCertificate.hs" ["unsupported construction compiler options"]
    rejectsBecause "SplicedDeclaration.hs" ["unsupported executable form: Template Haskell"]
    rejectsBecause "RebindableExpansion.hs" ["unsupported executable expansion"]
    rejectsBecause "ReboundDemand.hs" ["unsupported executable expansion"]
    rejectsBecause "SizedDemandOtherInput.hs" ["unsupported observation schema for Memorist.Data.Interface.Demand.arbDemandFrom"]
    rejectsBecause "SizedDemandEscape.hs" ["unapproved external operation Test.QuickCheck.Gen.sized"]
    rejectsBecause "SizedDemandDead.hs" ["unapproved runtime operation Memorist.Base.Core.eval1"]
    rejectsBecause "DatatypeGeneratorWrongRole.hs" ["unapproved external operation Test.QuickCheck.Gen.sized"]
    rejectsBecause "DatatypeGeneratorEscape.hs" ["unapproved external operation Test.QuickCheck.Gen.sized"]
    rejectsBecause "DatatypeGeneratorDead.hs" ["unapproved runtime operation Memorist.Base.Core.eval1"]
    rejectsBecause "DemandDerivingEscape.hs"
        ["DemandDerivingEscape.hs:8:", "DemandDerivingEscape.hs:11:", "demand-data methods require a checked observation declaration"]
    rejectsBecause "DemandManualInstance.hs" ["demand-data methods require a checked observation declaration"]
    rejectsBecause "DemandProtectedDeriving.hs" ["unapproved selected method GHC.Classes.==", "protected thunk/annotation/accounting operand"]
    mapM_ accepts
        ["ImportAliasControl.hs", "LocalShadowing.hs", "Construction.hs", "RecordUpdateControl.hs", "AnnotatedPatternControl.hs",
         "MethodControl.hs", "DemandControl.hs", "StructuralViewControl.hs", "SchemaViewControl.hs", "SectionExpansionControl.hs",
         "SizedDemandControl.hs", "DatatypeGeneratorControl.hs", "DemandDerivingControl.hs"]
    putStrLn "PASS: generated-construction / closed construction grammar, selected method evidence, complete home dependencies and checked observation roles / negative specimens typecheck independently / pre-caller guard"
  where
    specimen name = getDataFileName ("test" </> "generated-construction" </> name)
    accepts name = specimen name >>= \output -> typechecks output >> checkGeneratedConstruction output
    rejects name expected = do
        output <- specimen name
        typechecks output
        result <- try (checkGeneratedConstruction output) :: IO (Either IOException ())
        case result of
            Left problem -> forM_ expected $ \(line, operation) ->
                unless (any (\diagnostic -> all (`isInfixOf` diagnostic)
                        [output ++ ":" ++ show (line :: Int) ++ ":", "unapproved runtime operation " ++ operation,
                         "is forbidden in generated code"]) (lines (displayException problem))) $
                    fail (output ++ ": expected forbidden " ++ operation ++ ", got " ++ displayException problem)
            Right () -> fail (output ++ ": expected forbidden runtime operation, got Right ()")
    rejectsBecause name expected = do
        output <- specimen name
        typechecks output
        result <- try (checkGeneratedConstruction output) :: IO (Either IOException ())
        case result of
            Left problem | all (`isInfixOf` displayException problem) expected -> pure ()
            _ -> fail (output ++ ": expected " ++ show expected ++ ", got " ++ show result)
    typechecks output = withSystemTempDirectory "construction-specimen-" $ \objects -> do
        (status, out, err) <- readProcessWithExitCode "ghc"
            ["-v0", "-fno-code", "-fforce-recomp", "-i", "-i" ++ takeDirectory output,
             "-outputdir", objects, "-package", "MemoristANT", "-package", "QuickCheck", output] ""
        unless (status == ExitSuccess) (fail (output ++ ": specimen must typecheck independently before its verdict\n" ++ out ++ err))
