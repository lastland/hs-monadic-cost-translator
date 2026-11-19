module GeneratedConstruction (checkGeneratedConstruction, checkGeneratedConstructionSpecimens) where

import Control.Exception (IOException, try)
import Control.Monad (forM, unless, when)
import Cost.Translate (translateFile)
import Data.Data (Data)
import Data.Foldable (toList)
import Data.List (isInfixOf, isPrefixOf)
import ConstructionAst (collectAst)
import ConstructionEvidence (checkEvidence, methodNames)
import ConstructionHomeRepr (homeReprs)
import ConstructionRoles (supportCoreNames, homeDemandTypes)
import ConstructionRuntime (runtime)
import ConstructionSyntax (checkCompilerOptions, checkParsedSyntax, checkSyntax)
import ConstructionViews (viewSupport)
import GHC
    ( AmbiguousFieldOcc(..), GhcTc, HsExpr(..), HsFieldBind(..)
    , HsMatchContext(..), LHsRecUpdFields(..), Match(..), MatchGroup(..), Name, Pat(..), SrcSpan
    , GhcLink(NoLink), LoadHowMuch(LoadAllTargets), SuccessFlag(..)
    , backend, getLocA, getLogger
    , getSessionDynFlags, guessTarget, importPaths, mgModSummaries, ms_mod
    , coreModule, desugarModule, load, noBackend, parseDynamicFlags, parseModule, depanal
    , runGhc, setSessionDynFlags, setTargets, tm_renamed_source, tm_typechecked_source, typecheckModule
    , unLoc, ghcLink
    )
import GHC.Core.ConLike (conLikeName)
import GHC.Hs.Expr (HsRecUpdParent(..), HsThingRn(..), HsWrap(..), XXExprGhcTc(..))
import GHC.Unit.Module.ModGuts (mg_binds)
import GHC.Paths (libdir)
import GHC.Types.GREInfo (conLikeName_Name)
import GHC.Types.Name.Reader (greName)
import GHC.Types.SrcLoc (noLoc)
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import Paths_cost_translate (getDataFileName)
import System.FilePath ((</>), takeDirectory)
import System.Directory (copyFile, getTemporaryDirectory)
import System.Exit (ExitCode(..))
import System.IO.Error (ioeGetErrorString)
import System.IO.Temp (createTempDirectory, withSystemTempDirectory)
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
        programs <- mapM (fmap (mg_binds . coreModule) . desugarModule) checkedModules
        let checkedPrograms = zip checkedModules programs
            representations = homeReprs rt checkedPrograms
        (views, viewNames, viewCalls) <- viewSupport rt homes checkedPrograms
        fmap concat $ forM checkedPrograms $ \(checked, program) -> do
          case tm_renamed_source checked of
            Just (group, _, _, _, _) -> do
                let updates = ambiguousUpdateReferences (tm_typechecked_source checked)
                    (syntaxProblems, sites) = checkSyntax rt homes methods viewNames views viewCalls updates group
                evidenceProblems <- checkEvidence rt representations program (homeDemandTypes rt checkedModules) (supportCoreNames views checked) checked sites
                pure (syntaxProblems ++ evidenceProblems)
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
    , length fields == length (rnRecUpdLabels parent)
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

checkGeneratedConstructionSpecimens :: IO ()
checkGeneratedConstructionSpecimens = do
    rejectsCompleteBox
    rejectsAt "WrapperCarrier.hs" [(4, carrier "=<<"), (7, functorWrapper), (10, functorWrapper)]
    accepts "HomeReprUse.hs"
    rejectsAt "HomeReprUnsafe.hs" [(10, functorWrapper), (11, forbidden tval)]
    rejectsAt "HomeReprGiven.hs" [(8, selected "Memorist.Data.Interface.Data.reprM: reprM requires a concrete method-specific construction closure and audited carrier")]
    rejectsAt "HomeReprDelegation.hs" [(9, carrier "pure")]
    accepts "IntegerSummaryControl.hs"
    rejects "IntegerSummaryBypass.hs" [(7, "Memorist.Data.Interface.Data.abstract"), (10, "Memorist.Base.Thunk.tval")]
    rejectsAt "IntegerSummaryBypass.hs"
        [(13, protectedEq), (16, selected "GHC.Classes.==: unknown concrete scalar instance"), (19, carrier "pure")]
    accepts "OrdinarySummaryControl.hs"
    rejects "OrdinarySummaryBypass.hs"
        [(6, "Memorist.Base.Thunk.tval"), (9, "Memorist.Base.Core.value"), (12, "Memorist.Base.Thunk.tval")]
    rejectsAt "OrdinarySummaryBypass.hs" [(15, carrier "pure")]
    rejectsAt "GenericDictionary.hs"
        [(7, selected "GHC.Internal.Base.>>="), (10, selected "Memorist.Data.Interface.Data.reprM"),
         (13, selected "GHC.Internal.Num.+"), (19, selected "GenericDictionary.firstOrder")]
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
    rejectsAt "ExternalRecordUpdate.hs" [(10, external "Test.QuickCheck.Test.maxSuccess")]
    rejects "AnnotatedPattern.hs" [(6, "Memorist.Base.Core.Annotated"), (11, "Memorist.Base.Core.Annotated")]
    rejects "AnnotatedRecordPatterns.hs"
        [(7, "Memorist.Base.Core.Annotated"), (10, "Memorist.Base.Core.Annotated")
         ,(13, "Memorist.Base.Core.Annotated"), (16, "Memorist.Base.Core.Annotated")]
    rejects "InstancePayload.hs" [(10, "Memorist.Base.Thunk.tval")]
    rejectsAt "PureCarrier.hs" [(7, carrier "pure"), (10, carrier "pure")]
    rejectsAt "BindCarrier.hs" [(6, carrier ">>=")]
    rejectsAt "ApplicativeCarrier.hs" [(6, carrier "<*>")]
    rejectsAt "ProtectedEq.hs" [(6, protectedEq), (10, protectedEq)]
    rejectsAt "PatternPayload.hs" [(7, "unsupported executable form: pattern synonym")]
    rejectsAt "UnknownExternal.hs" [(6, external "GHC.Internal.Data.Typeable.Internal.someTypeRep")]
    rejects "UnknownRuntime.hs" [(7, "Memorist.Base.Extension.composeTM")]
    rejectsIn "ImportedWrapper.hs" "HiddenConstruction.hs" [(6, forbidden tval)]
    rejectsIn "DemandComputation.hs" "DemandControl.hs" [(16, forbidden tval)]
    rejects "AlteredDemandView.hs" [(12, tval)]
    rejects "ViewAlteredMetadata.hs" [(6, "Memorist.Base.Thunk.Th"), (9, "Memorist.Base.Thunk.Th")]
    rejectsAt "ViewArbitraryConverter.hs" [(7, view "preserve"), (10, view "preserve")]
    rejects "ViewDeadConverter.hs" [(7, eval1)]
    rejectsAt "ViewEscape.hs" [(8, view "preserve"), (11, view "preserve"), (14, view "spineView")]
    rejectsAt "ViewCrossInput.hs" [(7, view "preserve"), (11, view "preserve")]
    rejects "ViewRawPattern.hs" [(7, "Memorist.Base.Thunk.Th")]
    rejectsAt "SchemaIndependentFields.hs" [(15, view "preserve")]
    rejectsAt "SchemaChangedConstructor.hs" [(17, view "preserve")]
    rejectsAt "SchemaCallback.hs" [(8, view "preserve")]
    rejectsAt "SchemaEscape.hs" [(6, view "branch")]
    rejectsAt "SchemaTypeOnly.hs" [(12, view "preserve")]
    rejectsAt "SchemaOpaque.hs" [(16, view "preserve")]
    rejects "SchemaDeadHelper.hs" [(8, tval)]
    deferred <- specimen "DeferredCertificate.hs"
    typechecks deferred
    expectRejection deferred [(deferred ++ ":", "unsupported construction compiler options: deferred errors")]
    rejectsAt "SplicedDeclaration.hs" [(1, "unsupported executable form: Template Haskell")]
    rejectsAt "RebindableExpansion.hs" [(13, "unsupported executable expansion")]
    rejectsAt "ReboundDemand.hs" [(18, "unsupported executable expansion")]
    rejectsAt "SizedDemandOtherInput.hs" [(16, sized)]
    rejectsAt "SizedDemandEscape.hs" [(8, sized)]
    rejects "SizedDemandDead.hs" [(17, eval1)]
    rejectsAt "DatatypeGeneratorWrongRole.hs" [(11, sized)]
    rejectsDespiteImport "DatatypeGeneratorEscape.hs" 7 "DatatypeGeneratorControl.hs" 20 sized
    rejects "DatatypeGeneratorDead.hs" [(18, eval1)]
    rejectsAt "DemandDerivingEscape.hs" [(8, demandEq), (11, demandEq)]
    rejectsAt "DemandManualInstance.hs" [(15, demandEq)]
    rejectsAt "DemandProtectedDeriving.hs" [(13, protectedEq)]
    rejectsAt "SumGeneratorMissing.hs" [(15, oneof)]
    rejectsDespiteImport "SumGeneratorEscape.hs" 7 "SumGeneratorControl.hs" 23 oneof
    rejects "SumGeneratorDead.hs" [(16, eval1)]
    rejectsAt "SumTagEscape.hs" [(8, selected "GHC.Magic.dataToTag#: unknown executable method/default dependency")]
    rejectsAt "ExplicitMethodWrongCarrier.hs" [(5, carrier "pure")]
    rejectsAt "UnusedExplicitMethod.hs" [(8, carrier "pure")]
    rejectsAt "ForallClosureChanged.hs" [(10, view "preserve"), (13, view "preserve")]
    rejects "ConstraintWrapperEscape.hs" [(11, tval)]
    rejectsIn "ConstraintWrapperEscape.hs" "DemandControl.hs" [(16, forbidden tval)]
    rejects "ConstraintWrapperAltered.hs" [(16, tval)]
    rejects "ConstraintWrapperWrongProtocol.hs" [(17, tval)]
    rejects "ConstraintWrapperDead.hs" [(15, eval1)]
    rejects "Construction.hs" [(21, "Memorist.Data.Interface.Data.abstract"), (21, "Memorist.Base.Thunk.tval")]
    rejectsAt "MethodControl.hs" [(19, carrier ">>=" ++ "; generic dictionaries confer no permission")]
    rejects "DemandControl.hs" [(16, "Memorist.Base.Thunk.tval"), (19, "Memorist.Base.Thunk.tval")]
    rejectsAt "SizedDemandControl.hs"
        [(8, selected "Memorist.Data.Interface.Demand.$dmusage: runtime observation/accounting methods have no construction permission")]
    rejects "DatatypeGeneratorControl.hs"
        [(12, "Memorist.Data.Interface.Demand.fullDemand"), (13, "Memorist.Data.Interface.Demand.usage"),
         (13, "Memorist.Base.Thunk.union"), (13, "Memorist.Base.Thunk.empty")]
    rejectsAt "DemandDerivingControl.hs" [(12, forbidden "Memorist.Data.Interface.Demand.usage"), (14, demandEq)]
    rejects "SumGeneratorControl.hs"
        [(15, "Memorist.Base.Thunk.empty"), (16, "Memorist.Data.Interface.Demand.usage"), (16, "Memorist.Base.Thunk.union")]
    rejects "ConstraintWrapperControl.hs"
        [(19, "Memorist.Data.Interface.Data.abstract"), (19, "Memorist.Base.Thunk.tval"),
         (25, "Memorist.Data.Interface.Demand.usage"), (25, "Memorist.Base.Thunk.union"), (25, "Memorist.Base.Thunk.empty")]
    mapM_ accepts
        ["ImportAliasControl.hs", "LocalShadowing.hs", "ConcreteConstructionControl.hs", "RecordUpdateControl.hs", "AnnotatedPatternControl.hs",
          "StructuralViewControl.hs", "SchemaViewControl.hs", "SectionExpansionControl.hs", "TypeOnlyControl.hs",
          "ExplicitMethodControl.hs", "ForallClosureControl.hs"]
    putStrLn "PASS: generated-construction / closed construction grammar, concrete selected methods, complete home dependencies and checked structural views / no observation or generic-dictionary exemption / negative specimens typecheck independently / pre-caller guard"
  where
    specimen name = getDataFileName ("test" </> "generated-construction" </> name)
    tval = "Memorist.Base.Thunk.tval"
    eval1 = "Memorist.Base.Core.eval1"
    forbidden op = "unapproved runtime operation " ++ op ++ " is forbidden in generated code"
    external op = "unapproved external operation " ++ op ++ " is forbidden in generated code"
    selected op = "unapproved selected method " ++ op
    functorWrapper = selected "GHC.Internal.Data.Functor.<$>: requires the concrete audited Memorist instance"
    carrier op = selected ("GHC.Internal.Base." ++ op ++ ": requires the concrete audited Memorist instance")
    protectedEq = selected "GHC.Classes.==: protected thunk/annotation/accounting operand"
    demandEq = selected "GHC.Classes.==: demand-data methods have no observation exemption"
    view op = "unchecked structural view use/escape: " ++ op
    sized = external "Test.QuickCheck.Gen.sized"
    oneof = external "Test.QuickCheck.Gen.oneof"
    -- Select reports to mask by exact path/line, independently of the matcher
    -- under test. Keep the location and the import's same-operation report.
    rejectsDespiteImport name line dependency importedLine reason = do
        output <- specimen name
        imported <- specimen dependency
        typechecks output
        diagnostics <- rejection output
        let target = (site output line, reason)
            dependencySite = (site imported importedLine, reason)
            mask report | fst target `isPrefixOf` report = fst target ++ " unexpected operation"
                        | otherwise = report
            masked = unlines (map mask (lines diagnostics))
        requireDiagnostic diagnostics target
        requireDiagnostic masked dependencySite
        when (matchesDiagnostic target masked) $
            fail (output ++ ": dependency masked missing target operation: " ++ reason)
    rejectsCompleteBox = do
        temporary <- getTemporaryDirectory
        directory <- createTempDirectory temporary "construction-box-artifact-"
        putStrLn ("Complete Box construction artifact: " ++ directory)
        source <- getDataFileName ("test" </> "cases" </> "datatype-only-conversion" </> "Example.hs")
        let input = directory </> "Example.hs"
            output = directory </> "Generated.hs"
        copyFile source input
        translated <- translateFile input output
        either (fail . ("Box must translate before the construction verdict: " ++)) pure translated
        checkBox output
        -- Exercise the same verdict after a layout-only change to the artifact.
        shifted <- createTempDirectory directory "shifted-"
        copyFile input (shifted </> "Example.hs")
        let shiftedOutput = shifted </> "Generated.hs"
        readFile output >>= writeFile shiftedOutput . ('\n' :)
        checkBox shiftedOutput
    checkBox output = do
        typechecks output
        expectRejection output [(output ++ ":", forbidden op) | op <-
            [tval, "Memorist.Data.Interface.Data.abstract", "Memorist.Data.Interface.Demand.usage",
             "Memorist.Base.Thunk.empty", "Memorist.Base.Thunk.union"]]
    accepts name = specimen name >>= \output -> typechecks output >> checkGeneratedConstruction output
    rejects name expected = rejectsAt name [(line, forbidden op) | (line, op) <- expected]
    rejectsAt name = rejectsIn name name
    rejectsIn name target expected = do
        output <- specimen name
        path <- specimen target
        typechecks output
        expectAt output path expected
    expectAt output path expected = expectRejection output
        [(site path line, reason) | (line, reason) <- expected]
    site path line = path ++ ":" ++ show (line :: Int) ++ ":"
    expectRejection output expected = rejection output >>= \diagnostics -> mapM_ (requireDiagnostic diagnostics) expected
    rejection output = do
        result <- try (checkGeneratedConstruction output) :: IO (Either IOException ())
        case result of
            Left problem -> pure (ioeGetErrorString problem)
            Right () -> fail (output ++ ": expected rejection, got Right ()")
    requireDiagnostic diagnostics expected = unless (matchesDiagnostic expected diagnostics) $
        fail ("missing diagnostic " ++ show expected ++ ", got " ++ diagnostics)
    matchesDiagnostic (prefix, reason) = any (\line -> prefix `isPrefixOf` line && (": " ++ reason) `isInfixOf` line) . lines
    typechecks output = withSystemTempDirectory "construction-specimen-" $ \objects -> do
        (status, out, err) <- readProcessWithExitCode "ghc"
            ["-v0", "-fno-code", "-fforce-recomp", "-i", "-i" ++ takeDirectory output,
             "-outputdir", objects, "-package", "MemoristANT", "-package", "QuickCheck", output] ""
        unless (status == ExitSuccess) (fail (output ++ ": specimen must typecheck independently before its verdict\n" ++ out ++ err))
