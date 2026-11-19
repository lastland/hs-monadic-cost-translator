module GeneratedConstruction (checkGeneratedConstruction, checkGeneratedConstructionSpecimens) where

import Control.Exception (IOException, try)
import Control.Monad (forM, unless, void)
import Cost.Translate (translateFile)
import Data.Foldable (toList)
import Data.List (isInfixOf, isPrefixOf, stripPrefix)
import ConstructionAst (pretty)
import ConstructionEvidence (collectEvidence, checkEvidence, methodNames)
import ConstructionHomeRepr (homeReprs)
import ConstructionQuickCheckUses (quickCheckPackage, quickCheckResult, quickCheckUses)
import ConstructionRoles (quickCheckRoles, supportCoreNames, homeDemandTypes)
import ConstructionRuntime (resolveExport, runtime)
import ConstructionSyntax (checkCompilerOptions, checkParsedSyntax, checkSyntax)
import ConstructionViews (viewSupport)
import GeneratedFunctions (parseHaskellModule)
import GHC
    ( ClsInstDecl(..), GhcPs, HsBindLR(FunBind), HsDecl(..)
    , HsModule(..), HsType(..), InstDecl(ClsInstD)
    , Ghc, GhcLink(NoLink), LoadHowMuch(LoadAllTargets), SuccessFlag(..), TyThing(AnId)
    , backend, getLocA, getLogger, getModSummary, getName, idType, lookupName, mkModuleName
    , getSessionDynFlags, guessTarget, importPaths, mgModSummaries, ms_mod
    , coreModule, desugarModule, load, noBackend, parseDynamicFlags, parseModule, depanal
    , runGhc, setSessionDynFlags, setTargets, typecheckModule
    , unLoc, ghcLink
    )
import GHC.Builtin.Names (integralClassName)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.Type (getTyVar_maybe)
import GHC.Data.FastString (mkFastString)
import qualified GHC.Data.Strict as Strict
import GHC.Hs.Type (getLHsInstDeclHead, ignoreParens)
import GHC.Unit.Module.ModGuts (mg_binds)
import GHC.Paths (libdir)
import GHC.Tc.Utils.TcType (tcSplitSigmaTy)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SrcLoc
    ( BufPos(..), BufSpan(..), GenLocated(..), RealSrcSpan, SrcSpan(RealSrcSpan)
    , isRealSubspanOf, mkRealSrcLoc, mkRealSrcSpan, noLoc, srcSpanEndCol, srcSpanStartLine
    )
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
        configureArtifact output
        rt <- runtime
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
        methods <- methodNames rt checkedModules
        programs <- mapM (fmap (mg_binds . coreModule) . desugarModule) checkedModules
        let checkedPrograms = zip checkedModules programs
            representations = homeReprs rt checkedPrograms
        qcResult <- quickCheckResult
        qc <- quickCheckRoles qcResult checkedModules
        let uses = quickCheckUses qcResult programs
        (views, viewNames, viewCalls) <- viewSupport rt qc homes checkedPrograms
        reports <- forM checkedPrograms $ \(checked, program) -> do
            let (syntaxProblems, sites) = checkSyntax rt qc (uses checked) homes methods viewNames views viewCalls checked
            evidence <- collectEvidence rt qc representations program (supportCoreNames views checked) checked sites
            pure (syntaxProblems, evidence)
        pure (concatMap fst reports ++ checkEvidence rt representations (homeDemandTypes rt checkedModules) (concatMap snd reports))
    unless (null problems) (fail (unlines problems))

configureArtifact :: FilePath -> Ghc ()
configureArtifact output = do
    flags <- getSessionDynFlags
    logger <- getLogger
    -- Inspect the imported DFun fields selected by superclass projections.
    (configured, leftovers, _) <- parseDynamicFlags logger flags
        (map noLoc ["-fno-ignore-interface-pragmas", "-package", "MemoristANT", "-package", quickCheckPackage])
    unless (null leftovers) (fail "cannot configure generated-construction runtime packages")
    void $ setSessionDynFlags configured
        { backend = noBackend, ghcLink = NoLink, importPaths = [takeDirectory output] }

checkGeneratedConstructionSpecimens :: IO ()
checkGeneratedConstructionSpecimens = do
    checkIntegralContext "ClosedIntegral" ("constrained-local-disjoint-scoped" </> "ClosedIntegral.hs")
    checkIntegralContext "AtIntegralOwner" ("constrained-local-superclass-scoped" </> "AtIntegralOwner.hs")
    checksDiagnosticSites
    rejectsCompleteBox
    accepts "QuickCheckRoles.hs"
    checksQuickCheckMixed
    checksMixedMakeInstances
    mixed "QuickCheckArgumentProperty.hs" [TopBinding "gen", TopBinding "prop_transfer"] "QuickCheckArgumentConsumer.hs"
        [(8, qcValue "<local>.k")]
    mixed "QuickCheckValueUses.hs" [TopBinding "prop_force", TopBinding "first"] "QuickCheckValueUses.hs"
        [(9, qcValue "<local>.draw"), (17, qcValue "<local>.claim"), (24, qcValue "QuickCheckValueUses.first")]
    accepts "QuickCheckUnusedArgument.hs"
    mixed "QuickCheckStrictArgument.hs" [TopBinding "strict", TopBinding "prop_specialized"] "QuickCheckStrictArgument.hs"
        [(11, qcValue "QuickCheckStrictArgument.strict"), (14, qcValue "GHC.Prim.seq"),
         (17, "strict pattern forces a QuickCheck value outside a generator or property")]
    rejectsAt "WrapperCarrier.hs" [(4, carrier "=<<"), (7, functorWrapper), (10, functorWrapper)]
    accepts "HomeReprUse.hs"
    mapM_ accepts ["FieldProjection.hs", "FieldForward.hs", "FieldUnused.hs", "FieldChain.hs"]
    rejectsAt "FieldBadUse.hs" [(9, reprFailure), (12, reprFailure), (15, reprFailure), (18, reprFailure)]
    rejectsAt "FieldHidden.hs" [(9, reprFailure)]
    rejectsAt "FieldGrowth.hs" [(12, reprFailure)]
    mapM_ accepts ["RecurNat.hs", "NewtypeLabel.hs", "ScopeCarrier.hs"]
    accepts "RecurFlip.hs"
    rejectsAt "RecurFlipBad.hs" [(7, reprFailure)]
    rejectsAt "RecurOtherUse.hs" [(6, reprFailure)]
    rejectsAt "NewtypeLabelBad.hs" [(6, reprFailure)]
    rejectsAt "LabelFamilyList.hs" [(17, reprFailure)]
    accepts "LabelFamilyCast.hs"
    rejectsAt "ScopeCarrierBad.hs" [(11, reprFailure)]
    accepts "ContextReprUse.hs"
    rejectsAt "ContextReprCycle.hs" [(12, reprFailure)]
    rejectsAt "ContextReprBad.hs"
        [(8, reprFailure), (11, reprFailure), (14, reprFailure), (19, functorWrapper), (20, forbidden tval),
         (25, reprFailure), (31, selected "GHC.Classes.==: unknown concrete scalar instance"),
         (40, selected "ContextReprBad.inspect: unknown executable method/default dependency")]
    accepts "ParamReprUse.hs"
    rejectsAt "ParamReprBad.hs"
        [(8, reprFailure), (11, reprFailure), (16, functorWrapper), (17, forbidden tval),
         (35, reprFailure)]
    rejectsAt "HomeReprUnsafe.hs" [(10, functorWrapper), (11, forbidden tval)]
    rejectsAt "HomeReprGiven.hs" [(8, reprFailure)]
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
    -- Both projections are on line 6; only the filename distinguishes them.
    rejectsDespiteImport "PairedPayload.hs" 6 "HiddenConstruction.hs" 6 (forbidden tval)
    rejectsIn "DemandComputation.hs" "DemandControl.hs" [(19, forbidden tval)]
    mixed "AlteredDemandView.hs" [TopBinding "generateField"] "DemandControl.hs" [(19, forbidden tval)]
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
    options <- rejection deferred
    unless ((deferred ++ ": unsupported construction compiler options: deferred errors") `elem` lines options) $
        fail (deferred ++ ": missing deferred-errors rejection, got " ++ options)
    rejectsAt "SplicedDeclaration.hs" [(1, "unsupported executable form: Template Haskell")]
    rejectsAt "RebindableExpansion.hs" [(13, "unsupported executable expansion")]
    accepts "ReboundDemand.hs"
    mixed "SizedDemandOtherInput.hs" charGenerators "DemandControl.hs" [(19, forbidden tval)]
    mixed "SizedDemandEscape.hs" [TopBinding "ordinary"] "DemandControl.hs" [(19, forbidden tval)]
    rejects "SizedDemandDead.hs" [(14, eval1)]
    accepts "ArbitraryWithoutDemand.hs"
    rejectsDespiteImport "DatatypeGeneratorEscape.hs" 7 "DatatypeGeneratorControl.hs" 18 sized
    rejects "DatatypeGeneratorDead.hs" [(18, eval1)]
    rejectsAt "DemandDerivingEscape.hs" [(8, demandEq), (11, demandEq)]
    rejectsAt "DemandManualInstance.hs" [(15, demandEq)]
    rejectsAt "DemandProtectedDeriving.hs" [(13, protectedEq)]
    mixed "SumGeneratorMissing.hs" [InstanceMethod "Arbitrary" ["Observation"] "arbitrary"]
        "SumGeneratorMissing.hs" [(9, defaultUsage)]
    rejectsDespiteImport "SumGeneratorEscape.hs" 7 "SumGeneratorControl.hs" 25 oneof
    rejects "SumGeneratorDead.hs" [(17, eval1)]
    rejectsAt "SumTagEscape.hs" [(8, selected "GHC.Magic.dataToTag#: unknown executable method/default dependency")]
    rejectsAt "ExplicitMethodWrongCarrier.hs" [(5, carrier "pure")]
    rejectsAt "UnusedExplicitMethod.hs" [(8, carrier "pure")]
    rejectsAt "ErasedHomeMethod.hs" [(10, selected "QuickCheckRoles.make: unknown executable method/default dependency")]
    rejectsAt "ForallClosureChanged.hs" [(10, view "preserve"), (13, view "preserve")]
    rejects "ConstraintWrapperEscape.hs" [(11, tval)]
    rejectsIn "ConstraintWrapperEscape.hs" "DemandControl.hs" [(19, forbidden tval)]
    rejects "ConstraintWrapperAltered.hs" [(16, tval)]
    rejects "ConstraintWrapperWrongProtocol.hs" [(17, tval)]
    rejects "ConstraintWrapperDead.hs" [(15, eval1)]
    rejects "Construction.hs" [(21, "Memorist.Data.Interface.Data.abstract"), (21, "Memorist.Base.Thunk.tval")]
    rejectsAt "MethodControl.hs" [(19, carrier ">>=" ++ "; generic dictionaries confer no permission")]
    mixed "DemandControl.hs" [TopBinding "generateField"] "DemandControl.hs" [(19, forbidden tval)]
    mixed "SizedDemandControl.hs" charGenerators "SizedDemandControl.hs" [(8, defaultUsage)]
    rejects "DatatypeGeneratorControl.hs"
        [(13, "Memorist.Data.Interface.Demand.fullDemand"), (14, "Memorist.Data.Interface.Demand.usage"),
         (14, "Memorist.Base.Thunk.union"), (14, "Memorist.Base.Thunk.empty")]
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
    putStrLn "PASS: generated-construction / QuickCheck generators and properties, checked ordinary computations, observers and shrinkers / concrete selected methods, complete home dependencies and checked structural views / pre-caller guard"
  where
    specimen name = getDataFileName ("test" </> "generated-construction" </> name)
    tval = "Memorist.Base.Thunk.tval"
    eval1 = "Memorist.Base.Core.eval1"
    forbidden op = "unapproved runtime operation " ++ op ++ " is forbidden in generated code"
    external op = "unapproved external operation " ++ op ++ " is forbidden in generated code"
    qcValue name = "QuickCheck value used outside a generator or property: " ++ name
    selected op = "unapproved selected method " ++ op
    reprFailure = selected "Memorist.Data.Interface.Data.reprM: reprM requires a concrete method-specific construction closure and audited carrier"
    functorWrapper = selected "GHC.Internal.Data.Functor.<$>: requires the concrete audited Memorist instance"
    carrier op = selected ("GHC.Internal.Base." ++ op ++ ": requires the concrete audited Memorist instance")
    protectedEq = selected "GHC.Classes.==: protected thunk/annotation/accounting operand"
    demandEq = selected "GHC.Classes.==: demand-data methods have no observation exemption"
    defaultUsage = selected "Memorist.Data.Interface.Demand.$dmusage: runtime observation/accounting methods have no construction permission"
    view op = "unchecked structural view use/escape: " ++ op
    sized = external "Test.QuickCheck.Gen.sized"
    oneof = external "Test.QuickCheck.Gen.oneof"
    charGenerators = [InstanceMethod "Observation" ["Char"] "generate", InstanceMethod "DemandArb" ["Char"] "arbDemandFrom"]
    checksDiagnosticSites = do
        output <- specimen "MixedDiagnosticSites.hs"
        imported <- specimen "HiddenConstruction.hs"
        typechecks output
        diagnostics <- rejection output
        let range = "unsupported executable expression: [1 .. 3]"
            -- Keep the multiline expression and the unrelated import; remove
            -- the Core reports on the binder independently of either matcher.
            masked = unlines (filter (not . isInfixOf "unapproved selected method GHC.Internal.Enum.enumFromTo:") (lines diagnostics))
        requireAt masked imported [(6, forbidden tval)]
        cleanDefinitions diagnostics output [TopBinding "draw"]
        expectAssertionFailure ("unexpected diagnostic in steps: " ++ range)
            (cleanDefinitions masked output [TopBinding "steps"])
        requireAt masked output [(13, range)]
        expectAssertionFailure (output ++ ":12: missing diagnostic: " ++ range)
            (requireAt masked output [(12, range)])
    checksQuickCheckMixed = do
        -- A bare seq in ordinary code must identify its exempt operand.
        let escape name = "QuickCheck binding used outside a generator or property: " ++ name
        mixed "QuickCheckMixed.hs" [TopBinding "draw", TopBinding "claim"] "QuickCheckMixed.hs"
            [(22, forbidden tval), (25, forbidden tval),
             (28, escape "QuickCheckRoles.sampleField"), (29, escape "QuickCheckMixed.claim"),
             (30, external "Test.QuickCheck.Gen.MkGen"), (31, external "Test.QuickCheck.Property.MkProperty"),
             (34, external "Test.QuickCheck.Gen.unGen"), (37, external "GHC.Prim.coerce"),
             (40, external "GHC.Internal.Unsafe.Coerce.unsafeCoerce"),
             (44, external "Test.QuickCheck.Arbitrary.arbitrary"), (48, external "Test.QuickCheck.Arbitrary.arbitrary"),
             (51, selected "QuickCheckRoles.make: unknown executable method/default dependency"),
             (54, escape "QuickCheckRoles.choices")]
    checksMixedMakeInstances = do
        local <- specimen "MixedMakeInstances.hs"
        imported <- specimen "QuickCheckRoles.hs"
        typechecks local
        diagnostics <- rejection local
        let make tys = InstanceMethod "Make" tys "make"
        checkDefinitions diagnostics local
            [(make ["Draw", "Bool"], []), (make ["Char"], [external "Test.QuickCheck.Arbitrary.arbitrary"])]
        cleanDefinitions diagnostics imported [make ["Gen", "Int"], make ["Property"]]
    -- Select reports to mask by exact path/line, independently of the matcher
    -- under test. Keep the location and the import's same-operation report.
    rejectsDespiteImport name line dependency importedLine reason = do
        output <- specimen name
        imported <- specimen dependency
        typechecks output
        diagnostics <- rejection output
        let target = (line, reason)
            mask report | Just text <- stripPrefix (output ++ ":") report,
                          any (`isPrefixOf` text) [show line ++ ":", "(" ++ show line ++ ","] =
                            output ++ ":" ++ takeWhile (/= ' ') text ++ " unexpected operation"
                        | otherwise = report
            masked = unlines (map mask (lines diagnostics))
        requireAt diagnostics output [target]
        requireAt masked imported [(importedLine, reason)]
        expectAssertionFailure (output ++ ":" ++ show line ++ ": missing diagnostic: " ++ reason)
            (requireAt masked output [target])
    expectAssertionFailure message assertion = do
        result <- try @IOException assertion
        case result of
            Left problem | message `isInfixOf` ioeGetErrorString problem -> pure ()
            _ -> fail ("expected assertion failure: " ++ message ++ "\nGot " ++ show result)
    completeArtifact label fixture = do
        temporary <- getTemporaryDirectory
        directory <- createTempDirectory temporary "construction-artifact-"
        putStrLn ("Complete " ++ label ++ " construction artifact: " ++ directory)
        source <- getDataFileName ("test" </> "cases" </> fixture)
        let input = directory </> "Example.hs"
            output = directory </> "Generated.hs"
        copyFile source input
        translated <- translateFile input output
        either (fail . ((label ++ " must translate before artifact inspection: ") ++)) pure translated
        pure (directory, input, output)
    checkIntegralContext label fixture = do
        (_, _, output) <- completeArtifact label fixture
        signature <- runGhc (Just libdir) $ do
            configureArtifact output
            target <- guessTarget output Nothing Nothing
            setTargets [target]
            Succeeded <- load LoadAllTargets
            home <- ms_mod <$> getModSummary (mkModuleName "Generated")
            name <- resolveExport home "viewsM"
            Just (AnId binder) <- lookupName name
            pure (idType binder)
        case tcSplitSigmaTy signature of
            ([a], [predicate], _) | Just (cls, [argument]) <- getClassPredTys_maybe predicate,
                getName cls == integralClassName, getTyVar_maybe argument == Just a ->
                    putStrLn ("PASS: " ++ label ++ " / exported viewsM requires only Integral a")
            _ -> fail (output ++ ": exported viewsM must require only Integral a; found " ++ pretty signature)
    rejectsCompleteBox = do
        (directory, input, output) <- completeArtifact "Box" ("datatype-only-conversion" </> "Example.hs")
        definitions <- definitionSpans <$> parseHaskellModule output
        checkBox output definitions
        -- Split the abstract equation after its binder, preserving the columns
        -- of its remaining tokens. The required observations move off that line.
        shifted <- createTempDirectory directory "shifted-"
        copyFile input (shifted </> "Example.hs")
        let shiftedOutput = shifted </> "Generated.hs"
        (_, binder) <- definition output definitions (InstanceMethod "Repr" ["Box"] "abstract")
        case binder of
            RealSrcSpan location (Strict.Just bytes) -> do
                (before, after) <- splitAt (bufPos (bufSpanEnd bytes)) <$> readFile output
                writeFile shiftedOutput ("\n" ++ before ++ "\n" ++ replicate (srcSpanEndCol location - 1) ' ' ++ after)
            _ -> fail (output ++ ": missing source offset of abstract")
        parseHaskellModule shiftedOutput >>= checkBox shiftedOutput . definitionSpans
    checkBox output definitions = do
        typechecks output
        reports <- rejection output >>= siteReports output
        let check = checkDefinition output definitions reports
        mapM_ check
            [(InstanceMethod "Repr" ["Box"] "abstract", map forbidden [tval, "Memorist.Data.Interface.Data.abstract"]),
             (InstanceMethod "Demand" ["BoxT"] "fullDemand", [forbidden "Memorist.Data.Interface.Demand.fullDemand"]),
             (InstanceMethod "Demand" ["BoxT"] "usage", map forbidden ["Memorist.Data.Interface.Demand.usage", "Memorist.Base.Thunk.empty", "Memorist.Base.Thunk.union"]),
             (TopBinding "shrinkField", [forbidden tval, external "Test.QuickCheck.Arbitrary.liftShrink"])]
        mapM_ (\key -> check (key, []))
            [TopBinding "arbitraryField", TopBinding "genField", TopBinding "genBoxDemand",
             InstanceMethod "GeneratedDemand" ["BoxT"] "genDemandBudget", InstanceMethod "Arbitrary" ["BoxD"] "arbitrary",
             InstanceMethod "DemandArb" ["BoxT"] "arbDemandFrom"]
    accepts name = specimen name >>= \output -> typechecks output >> checkGeneratedConstruction output
    rejects name expected = rejectsAt name [(line, forbidden op) | (line, op) <- expected]
    rejectsAt name = rejectsIn name name
    rejectsIn name = mixed name []
    mixed name clean target expected = do
        output <- specimen name
        path <- specimen target
        typechecks output
        diagnostics <- rejection output
        requireAt diagnostics path expected
        cleanDefinitions diagnostics output clean
    cleanDefinitions diagnostics output keys = checkDefinitions diagnostics output [(key, []) | key <- keys]
    checkDefinitions diagnostics output expected = unless (null expected) $ do
        definitions <- definitionSpans <$> parseHaskellModule output
        reports <- siteReports output diagnostics
        mapM_ (checkDefinition output definitions reports) expected
    requireAt diagnostics path expected = do
        reports <- siteReports path diagnostics
        let require target@(line, reason) = unless (hasSite target reports) $
                fail (path ++ ":" ++ show line ++ ": missing diagnostic: " ++ reason ++
                    case [actual | (at, actual) <- reports, srcSpanStartLine at == line] of
                        [] -> "\nNo diagnostic starts on that line."
                        found -> "\nOther reports on this line:\n" ++ unlines found)
        mapM_ require expected
    hasSite (line, reason) = any (\(at, actual) -> srcSpanStartLine at == line && matchesReason reason actual)
    rejection output = do
        result <- try (checkGeneratedConstruction output) :: IO (Either IOException ())
        case result of
            Left problem -> pure (ioeGetErrorString problem)
            Right () -> fail (output ++ ": expected rejection, got Right ()")
    typechecks output = withSystemTempDirectory "construction-specimen-" $ \objects -> do
        (status, out, err) <- readProcessWithExitCode "ghc"
            ["-v0", "-fno-code", "-fforce-recomp", "-i", "-i" ++ takeDirectory output,
             "-outputdir", objects, "-package", "MemoristANT", "-package", quickCheckPackage, output] ""
        unless (status == ExitSuccess) (fail (output ++ ": specimen must typecheck independently before its verdict\n" ++ out ++ err))

data SpecimenDefinition = TopBinding String | InstanceMethod String [String] String deriving Eq

type BindingSpans = [(SpecimenDefinition, SrcSpan, SrcSpan)] -- whole definition, then binder
type SiteReport = (RealSrcSpan, String)

instance Show SpecimenDefinition where
    show (TopBinding name) = name
    show (InstanceMethod cls args name) = name ++ " (instance " ++ cls ++ " " ++ ty ++ ")"
      where ty = case args of [one] -> one; _ -> "(" ++ unwords args ++ ")"

-- Some checks specify the operation or its reason ahead of concrete evidence.
-- A longer identifier with the same prefix must not satisfy that expectation.
matchesReason :: String -> String -> Bool
matchesReason expected actual = case stripPrefix expected actual of
    Just rest -> null rest || any (`isPrefixOf` rest) [" ", ": ", "; "]
    Nothing -> False

definition :: FilePath -> BindingSpans -> SpecimenDefinition -> IO (RealSrcSpan, SrcSpan)
definition path definitions key = case
        [(body, binder) | (name, RealSrcSpan body _, binder) <- definitions, key == name] of
    [entry] -> pure entry
    matches -> fail (path ++ ": expected one definition of " ++ show key ++ ", found " ++ show (length matches))

checkDefinition :: FilePath -> BindingSpans -> [SiteReport] -> (SpecimenDefinition, [String]) -> IO ()
checkDefinition path definitions reports (key, expected) = do
    (body, _) <- definition path definitions key
    let actual = [(at, reason) | (at, reason) <- reports, at `isRealSubspanOf` body]
    case (expected, actual) of
        ([], (at, reason):_) -> fail (pretty at ++ ": unexpected diagnostic in " ++ show key ++ ": " ++ reason)
        _ -> mapM_ (\reason -> unless (any (matchesReason reason . snd) actual) $
            fail (pretty body ++ ": missing diagnostic in " ++ show key ++ ": " ++ reason)) expected

-- Only a definition's binder counts, never a use in another function or a
-- method with the same spelling in an instance for another type.
definitionSpans :: HsModule GhcPs -> BindingSpans
definitionSpans parsed =
    [(TopBinding (occ name), getLocA decl, getLocA name)
    | decl@(L _ (ValD _ (FunBind _ name _))) <- hsmodDecls parsed] ++
    [(InstanceMethod (occ cls) tys (occ name), getLocA binding, getLocA name)
    | L _ (InstD _ (ClsInstD _ inst)) <- hsmodDecls parsed,
      HsAppTy _ lhs rhs <- [unLoc (ignoreParens (getLHsInstDeclHead (cid_poly_ty inst)))],
      HsTyVar _ _ cls <- [unLoc (ignoreParens lhs)], Just tys <- [typeNames rhs],
      binding@(L _ (FunBind _ name _)) <- toList (cid_binds inst)]
  where
    occ = occNameString . rdrNameOcc . unLoc
    typeNames ty = case unLoc (ignoreParens ty) of
        HsTyVar _ _ n -> Just [occ n]
        HsAppTy _ f x | HsTyVar _ _ n <- unLoc (ignoreParens x) -> (++ [occ n]) <$> typeNames f
        _ -> Nothing

-- The guard exposes GHC's printed positions: a point, a one-line range, or
-- a multi-line range. Its printed end column is inclusive; RealSrcSpan's is not.
siteReports :: FilePath -> String -> IO [SiteReport]
siteReports path diagnostics = forM
    [text | report <- lines diagnostics, Just text <- [stripPrefix (path ++ ":") report]] $ \text ->
        case break (== ' ') text of
            (location, ' ':reason) | Just (start, (line, column)) <- coordinates location ->
                pure (mkRealSrcSpan (point start) (point (line, column + 1)), reason)
            _ -> fail (path ++ ": cannot read diagnostic position: " ++ text)
  where
    point = uncurry (mkRealSrcLoc (mkFastString path))
    coordinates text = case reads text of
        [(start, '-':rest)] | [(end, ":")] <- reads rest -> Just (start, end)
        _ -> case reads text of
            [(line, ':':rest)] | [(column, ":")] <- reads rest -> Just ((line, column), (line, column))
                | [(first, '-':end)] <- reads rest, [(lastCol, ":")] <- reads end -> Just ((line, first), (line, lastCol))
            _ -> Nothing
