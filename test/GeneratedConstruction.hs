module GeneratedConstruction (checkGeneratedConstruction, checkGeneratedConstructionSpecimens) where

import Control.Exception (IOException, try)
import Control.Monad (forM, unless, void, when)
import Cost.Translate (translateFile)
import Data.Foldable (toList)
import Data.List (isInfixOf, isPrefixOf, stripPrefix)
import ConstructionAllocation (checkPhantomAllocation)
import ConstructionAst (collectAst, pretty)
import ConstructionEvidence (collectEvidence, checkEvidence, methodNames)
import ConstructionHomeRepr (homeReprs)
import ConstructionLocals (localBindings, independentBinding)
import ConstructionOperandTransfers (ordinaryOperandTransfers)
import ConstructionQuickCheckTypes (checkedHome)
import ConstructionQuickCheckUses (quickCheckPackage, quickCheckResult, quickCheckUses)
import ConstructionRoles (quickCheckRoles, homeDemandTypes, supportRoles, demandShrinkMethods, supportValues)
import ConstructionRuntime (resolveExport, resolveValue, runtime)
import ConstructionSyntax (checkCompilerOptions, checkParsedSyntax, checkSyntax)
import ConstructionViews (coreViewNames, viewSupport)
import GeneratedFunctions (parseHaskellModule)
import GHC
    ( ClsInstDecl(..), GRHS(..), GRHSs(..), GhcPs, HsBindLR(FunBind), HsDecl(..)
    , HsModule(..), HsType(..), InstDecl(ClsInstD), LHsBind, LPat, Match(..), MatchGroup(..), Pat(BangPat)
    , Ghc, GhcLink(NoLink), LoadHowMuch(LoadAllTargets), ModSummary, SuccessFlag(..), TyThing(AnId)
    , backend, findModule, getLocA, getLogger, getModSummary, getName, idType, lookupName, mkModuleName
    , getSessionDynFlags, guessTarget, importPaths, mgModSummaries, ms_mod
    , coreModule, desugarModule, load, noBackend, parseDynamicFlags, parseModule, depanal
    , runGhc, setSessionDynFlags, setTargets, typecheckModule
    , tm_renamed_source, unLoc, ghcLink
    )
import GHC.Builtin.Names (integralClassName, numClassName)
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
import System.FilePath ((</>), takeBaseName, takeDirectory)
import System.Directory (copyFile, getTemporaryDirectory)
import System.Exit (ExitCode(..))
import System.IO.Error (ioeGetErrorString)
import System.IO.Temp (createTempDirectory, withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

-- Resolve actual runtime definitions, not an import's spelling or qualifier.
-- The renamed tree retains references in unused local/top-level helpers.
checkGeneratedConstruction :: FilePath -> IO ()
checkGeneratedConstruction output = do
    problems <- constructionReports output
    unless (null problems) (fail (unlines problems))

constructionReports :: FilePath -> IO [String]
constructionReports output =
    runGhc (Just libdir) $ do
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
        qcResult <- quickCheckResult checkedModules
        qc <- quickCheckRoles qcResult checkedModules
        shrinkers <- demandShrinkMethods rt checkedModules
        let roles = supportRoles rt qc shrinkers checkedPrograms
            utilities = supportValues (independentBinding locals) checkedModules roles
            uses checked = quickCheckUses (qcResult (checkedHome checked)) checked
            locals = localBindings rt qc roles checkedModules
        views <- viewSupport rt locals homes checkedPrograms
        reports <- forM checkedPrograms $ \(checked, program) -> do
            let (syntaxProblems, sites) = checkSyntax rt utilities locals uses homes methods views checked
            evidence <- collectEvidence rt utilities locals representations program (coreViewNames views checked) checked sites
            pure (syntaxProblems, evidence)
        pure (concatMap fst reports ++ ordinaryOperandTransfers qcResult qc checkedModules ++
            checkEvidence rt representations (homeDemandTypes rt checkedModules) (concatMap snd reports))

configureArtifact :: FilePath -> Ghc ()
configureArtifact output = do
    flags <- getSessionDynFlags
    logger <- getLogger
    -- Inspect the imported DFun fields selected by superclass projections.
    (configured, leftovers, _) <- parseDynamicFlags logger flags
        (map noLoc ["-fno-ignore-interface-pragmas", "-package", "MemoristANT", "-package", quickCheckPackage,
            "-package", "ghc-prim"])
    unless (null leftovers) (fail "cannot configure generated-construction runtime packages")
    void $ setSessionDynFlags configured
        { backend = noBackend, ghcLink = NoLink, importPaths = [takeDirectory output] }

withLoadedGenerated :: FilePath -> (ModSummary -> Ghc a) -> IO a
withLoadedGenerated output inspect = runGhc (Just libdir) $ do
    configureArtifact output
    target <- guessTarget output Nothing Nothing
    setTargets [target]
    Succeeded <- load LoadAllTargets
    getModSummary (mkModuleName "Generated") >>= inspect

checkGeneratedConstructionSpecimens :: IO ()
checkGeneratedConstructionSpecimens = do
    accepts "CallerArithmetic.hs"
    accepts "CallerArithmeticOperations.hs"
    rejectsAt "CallerArithmeticUnsafe.hs"
        [(7, forbidden tval), (10, selected "GHC.Classes.==: unknown concrete scalar instance"),
         (13, selected "GHC.Classes.<: unknown concrete scalar instance"),
         (16, selected "GHC.Internal.Show.show: unknown concrete scalar instance"),
         (19, selected "GHC.Internal.Num.abs: unknown concrete scalar instance"),
         (22, selected "GHC.Internal.Num.+: protected thunk/annotation/accounting operand")]
    rejectsIn "CallerArithmeticImported.hs" "CallerArithmeticInstance.hs" [(8, forbidden tval)]
    rejectsAt "CallerArithmeticShadow.hs" [(9, unknown "CallerArithmeticShadow.+")]
    checksGeneratedCallerArithmetic
    accepts "ShrinkerLocalValue.hs"
    rejectsAt "ShrinkerNestedOrdinary.hs" [(17, forbidden tval), (18, forbidden tval)]
    mappedSupport <- specimen "MappedSupportGraph.hs"
    typechecks mappedSupport
    rejection mappedSupport >>= onlySite "SharedSupport.hs" (50, forbidden tval)
    accepts "StandaloneDemandShrink.hs"
    rejectsAt "StandaloneDemandShrinkBypass.hs"
        [(7, unknown "StandaloneDemandShrink.shrinkDemandCompatible"),
         (7, supportUse "StandaloneDemandShrink.shrinkDemandCompatible"),
         (10, supportUse "StandaloneDemandShrink.standalone"),
         (13, supportUse "StandaloneDemandShrink.shrinkField")]
    accepts "BoxSupport.hs"
    rejectsAt "BoxSupportBypass.hs"
        [(7, forbidden tval), (10, forbidden "Memorist.Data.Interface.Data.abstract"),
         (13, forbidden "Memorist.Data.Interface.Demand.fullDemand"),
         (15, forbidden "Memorist.Data.Interface.Demand.usage")]
    accepts "BoxSupportHelpers.hs"
    rejectsIn "BoxSupportHelperEscape.hs" "BoxSupportHelpers.hs" [(11, forbidden tval)]
    accepts "BoxSupportAccounting.hs"
    rejectsAt "BoxSupportSeparation.hs"
        [(11, forbidden tval), (20, forbidden tval), (23, forbidden tval),
         (25, forbidden "Memorist.Data.Interface.Data.abstract")]
    accepts "ProxyWitness.hs"
    rejectsAt "ProxyWitnessEscape.hs" [(8, forbidden tval)]
    accepts "DemandScalarMethods.hs"
    accepts "DemandConcreteRecursive.hs"
    accepts "DemandConcreteSum.hs"
    accepts "DemandConcreteLargeSum.hs"
    rejectsAt "DemandRecursiveProtected.hs"
        [(10, selected "GHC.Classes.==: unknown concrete scalar instance"),
         (13, selected "GHC.Internal.Show.show: unknown concrete scalar instance")]
    rejectsAt "DemandNestedMethod.hs"
        [(6, demandEq), (9, selected "GHC.Internal.Show.show: demand-data methods have no observation exemption")]
    rejectsAt "TagForged.hs"
        [(16, unknown "GHC.Magic.dataToTag#"), (22, unknown "GHC.Magic.dataToTag#"),
         (17, external "GHC.Internal.Unsafe.Coerce.unsafeCoerce"),
         (23, external "GHC.Internal.Unsafe.Coerce.unsafeCoerce")]
    rejectsAt "DemandScalarUnsafe.hs"
        [(7, forbidden tval), (9, forbidden tval),
         (12, demandEq), (14, selected "GHC.Internal.Show.show: demand-data methods have no observation exemption"),
         (17, protectedEq), (19, selected "GHC.Internal.Show.show: protected thunk/annotation/accounting operand"),
         (22, demandEq), (24, selected "GHC.Internal.Show.show: demand-data methods have no observation exemption"),
         (32, selected "GHC.Classes.==: unknown concrete scalar instance"),
         (34, selected "GHC.Internal.Show.show: unknown concrete scalar instance"),
         (37, selected "GHC.Internal.Show.show: protected thunk/annotation/accounting operand"),
         (40, protectedEq)]
    rejectsAt "DemandScalarShadow.hs"
        [(15, unknown "DemandScalarShadow.same"), (17, unknown "DemandScalarShadow.render")]
    rejectsAt "DemandScalarForged.hs" [(11, external "GHC.Internal.Unsafe.Coerce.unsafeCoerce"), (14, demandEq)]
    checksGeneratedDemandMethods
    checksPhantomProducer
    rejectsAt "BoxedAction.hs" [(13, forbidden tval)]
    accepts "BoxedActionControl.hs"
    rejectsAt "ActionContainer.hs" [(16, forbidden tval), (25, forbidden tval), (34, forbidden tval)]
    -- Caller arithmetic is supported; these retention controls instead use
    -- unrestricted Eq, whose methods still need concrete scalar evidence.
    rejectsAt "ActionFieldEvidence.hs" [(10, selected "GHC.Classes.==: unknown concrete scalar instance")]
    checksImplicitForcing
    checkIntegralContext ("constrained-local-disjoint-scoped" </> "ClosedIntegral.hs")
    checkIntegralContext ("constrained-local-superclass-scoped" </> "AtIntegralOwner.hs")
    checkIntegralContext ("inferred-owner-residual" </> "Example.hs")
    checkIntegralContext ("inferred-owner-residual" </> "UnrelatedShadow.hs")
    accepts "NestedPatternConsumption.hs"
    mixed "NestedPatternOrdinary.hs"
        [LocalBinding (TopBinding owner) "observed" | owner <- ["independentFunction", "unitWithHelper"]] "NestedPatternOrdinary.hs"
        [(10, selected "GHC.Classes.==: unknown concrete scalar instance"),
         (15, forbidden tval), (21, forbidden tval), (28, forbidden tval),
         (34, forbidden tval), (39, forbidden tval), (44, forbidden tval),
         (49, forbidden tval), (54, forbidden tval), (60, forbidden tval), (61, protectedEq),
         (67, forbidden tval), (72, forbidden tval), (77, forbidden tval), (84, forbidden tval),
         (90, selected "GHC.Classes.==: unknown concrete scalar instance [given Eq"),
         (96, selected "GHC.Classes.==: unknown concrete scalar instance [given Eq"), (101, forbidden tval)]
    checksDiagnosticSites
    checksCompleteBox
    accepts "QuickCheckRoles.hs"
    checksQuickCheckMixed
    checksMixedMakeInstances
    checksOperandPropertyCall
    accepts "OperandBoxControl.hs"
    mixed "OperandBoxUse.hs" [TopBinding "prop_packed"] "OperandBoxUse.hs"
        [(8, qcValue "OperandBoxControl.Box")]
    mixed "OperandNamedBang.hs" (map TopBinding ["first", "atBool", "prop_local", "project", "prop_project", "prop_callback"]) "OperandNamedBang.hs"
        [(22, qcValue "OperandNamedBang.first"), (25, qcValue "OperandNamedBang.first"),
         (34, "strict binding forces a QuickCheck value outside a generator or property")]
    mixed "OperandUpdateAlternatives.hs" (map TopBinding ["prop_other", "changeOther"]) "OperandUpdateAlternatives.hs"
        [(17, qcValue "OperandUpdateAlternatives.changeMixed")]
    mixed "OperandWrappedProperty.hs" (map TopBinding ["draw", "prop_own", "prop_scalar"]) "OperandWrappedProperty.hs"
        [(10, wrappedForce "law"), (11, wrappedForce "relay")]
    checksFamilyOperands
    mixed "OperandInstantiations.hs" [TopBinding "prop_pair"] "OperandInstantiations.hs"
        [(10, qcValue "OperandEquations.forcePair"), (13, qcValue "OperandEquations.strictF"),
         (16, qcValue "OperandEquations.erased"), (23, qcValue "OperandEquations.relay"),
         (27, qcValue "OperandEquations.pairAlias")]
    accepts "OperandIgnored.hs"
    mixed "OperandFieldSyntax.hs" (map TopBinding ["labelled", "boxed", "paired", "replaceSoft"]) "OperandFieldSyntax.hs"
        [(17, qcValue "<local>.p"), (20, qcValue "<local>.p"), (28, qcValue "<local>.p")]
    mixed "OperandForceSites.hs" (map TopBinding ["prop_first", "prop_lazy", "prop_second", "prop_rebound"]) "OperandForceSites.hs"
        [(33, qcValue "OperandForceSites.first"), (36, qcValue "OperandForceSites.strictId"),
         (37, qcValue "OperandForceSites.bangFirst")]
    mixed "OperandConstructorSites.hs" (map TopBinding ["prop_lazy", "prop_unchanged"]) "OperandConstructorSites.hs"
        [(20, qcValue "OperandConstructorSites.build"), (21, qcValue "OperandConstructorSites.buildRecord"),
         (22, qcValue "OperandConstructorSites.replaceHard")]
    mixed "QuickCheckArgumentProperty.hs" [TopBinding "gen", TopBinding "prop_transfer"] "QuickCheckArgumentConsumer.hs"
        [(8, qcValue "<local>.k")]
    mixed "QuickCheckValueUses.hs" [TopBinding "prop_force", TopBinding "first"] "QuickCheckValueUses.hs"
        [(9, qcValue "<local>.draw"), (17, qcValue "<local>.claim"), (24, qcValue "QuickCheckValueUses.first")]
    accepts "QuickCheckUnusedArgument.hs"
    mixed "QuickCheckFieldUse.hs" [TopBinding "ignoreItem", TopBinding "readBool"] "QuickCheckFieldUse.hs"
        [(9, qcValue "QuickCheckFieldUse.item")]
    accepts "QuickCheckMappedLines.hs"
    mappedViews <- specimen "MappedViewGraph.hs"
    typechecks mappedViews
    rejection mappedViews >>= onlySite "SharedViewInput.hs" (50, forbidden tval)
    checksMappedBang
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
         (40, unknown "ContextReprBad.inspect")]
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
    mixed "GenericDictionary.hs" [TopBinding "genericScalar"] "GenericDictionary.hs"
        [(7, selected "GHC.Internal.Base.>>="), (10, selected "Memorist.Data.Interface.Data.reprM"),
         (19, selected "GenericDictionary.firstOrder")]
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
    accepts "AlteredDemandView.hs"
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
    specimen "DeferredCertificate.hs" >>= rejectsOption "deferred errors"
    rejectsAt "SplicedDeclaration.hs" [(1, "unsupported executable form: Template Haskell")]
    rejectsAt "RebindableExpansion.hs" [(13, "unsupported executable expansion")]
    accepts "ReboundDemand.hs"
    accepts "SizedDemandOtherInput.hs"
    accepts "SizedDemandEscape.hs"
    rejects "SizedDemandDead.hs" [(14, eval1)]
    accepts "ArbitraryWithoutDemand.hs"
    rejectsDespiteImport "DatatypeGeneratorEscape.hs" 7 "DatatypeGeneratorControl.hs" 18 sized
    rejects "DatatypeGeneratorDead.hs" [(18, eval1)]
    mixed "DemandDerivingEscape.hs" [TopBinding "ordinary"] "DemandDerivingEscape.hs" [(11, demandEq)]
    mixed "DemandManualInstance.hs" (InstanceMethod "Eq" ["Observation"] "==" : demandProducer)
        "DemandManualInstance.hs" [(14, demandEq)]
    rejectsAt "DemandProtectedDeriving.hs" [(13, protectedEq)]
    accepts "SumGeneratorMissing.hs"
    rejectsDespiteImport "SumGeneratorEscape.hs" 7 "SumGeneratorControl.hs" 25 oneof
    rejects "SumGeneratorDead.hs" [(17, eval1)]
    rejectsAt "SumTagEscape.hs" [(8, external "GHC.Types.I#"), (8, external "GHC.Magic.dataToTag#")]
    accepts "DefaultLabels.hs"
    mixed "DefaultLabelSelection.hs" [TopBinding "inherited"] "DefaultLabelSelection.hs" $
        [(14, forbidden tval), (17, unknown "DefaultLabels.labels"), (20, unknown "DefaultLabels.labels" ++ " [given Labels")] ++
        [(line, unknown "DefaultLabels.$dmlabels") | line <- [23, 26, 29]]
    rejectsAt "ErasedDataDefault.hs" [(9, forbidden tval), (11, unknown "ErasedDataDefault.$dmmarks")]
    rejectsAt "ExplicitMethodWrongCarrier.hs" [(5, carrier "pure")]
    rejectsAt "UnusedExplicitMethod.hs" [(8, carrier "pure")]
    rejectsAt "ErasedHomeMethod.hs" [(10, unknown "QuickCheckRoles.make")]
    rejectsAt "ForallClosureChanged.hs" [(10, view "preserve"), (13, view "preserve")]
    rejects "ConstraintWrapperEscape.hs" [(11, tval)]
    -- abstract's permission does not depend on a constraint-wrapper protocol.
    accepts "ConstraintWrapperAltered.hs"
    accepts "ConstraintWrapperWrongProtocol.hs"
    rejects "ConstraintWrapperDead.hs" [(15, eval1)]
    accepts "Construction.hs"
    rejectsAt "MethodControl.hs" [(19, carrier ">>=" ++ "; generic dictionaries confer no permission")]
    accepts "DemandControl.hs"
    accepts "SizedDemandControl.hs"
    mixed "DatatypeGeneratorControl.hs" demandProducer "DatatypeGeneratorControl.hs" [(18, sized)]
    accepts "DemandDerivingControl.hs"
    mixed "SumGeneratorControl.hs" demandProducer "SumGeneratorControl.hs" [(25, oneof)]
    mixed "ConstraintWrapperControl.hs"
        [InstanceMethod "Repr" ["Source"] "abstract", InstanceMethod "Demand" ["Target"] "fullDemand",
         InstanceMethod "Demand" ["Target"] "usage", InstanceMethod "Observation" ["Target"] "shrinkCompatible"]
        "ConstraintWrapperControl.hs" [(18, reprFailure)]
    mapM_ accepts
        ["ImportAliasControl.hs", "LocalShadowing.hs", "ConcreteConstructionControl.hs", "RecordUpdateControl.hs", "AnnotatedPatternControl.hs",
          "StructuralViewControl.hs", "SchemaViewControl.hs", "SectionExpansionControl.hs", "TypeOnlyControl.hs",
          "ExplicitMethodControl.hs", "ForallClosureControl.hs"]
    mixed "NestedOrdinaryWhere.hs" [TopBinding "claim"] "NestedOrdinaryWhere.hs" [(9, forbidden tval)]
    accepts "NestedQuickCheckControl.hs"
    mixed "NestedOrdinaryLet.hs" [LocalBinding (TopBinding "claim") "draw", LocalBinding (TopBinding "claim") "check"] "NestedOrdinaryLet.hs"
        [(11, carrier "pure"), (12, carrier "pure"), (14, forbidden tval), (15, forbidden tval), (20, forbidden tval)]
    mixed "LocalImplicitParameter.hs" [TopBinding "law", TopBinding "constant"] "LocalImplicitParameter.hs"
        [(9, forbidden tval), (16, forbidden tval), (19, forbidden tval),
         (23, carrier "pure"), (24, carrier "pure"), (28, forbidden tval)]
    mixed "ErasedImplicitParameter.hs" [TopBinding "control"] "ErasedImplicitParameter.hs"
        [(10, selected "GHC.Classes.==: unknown concrete scalar instance [given Eq")]
    accepts "ImplicitValueControl.hs"
    rejectsAt "ImplicitValueOrdinary.hs" [(line, forbidden tval) | line <- [8, 15, 18, 27, 30, 33, 39, 45]]
    accepts "LexicalValueControl.hs"
    mixed "IndependentTypedLocals.hs" [TopBinding "run", TopBinding "lawful"] "IndependentTypedLocals.hs"
        [(9, forbidden tval), (14, selected "GHC.Internal.Enum.maxBound: unknown executable method/default dependency [given Bounded"),
         (15, forbidden tval), (24, forbidden tval), (36, forbidden tval), (43, forbidden tval)]
    rejectsAt "NestedOrdinaryView.hs" [(11, view "preserve")]
    mixed "NestedOrdinaryPattern.hs" [TopBinding "law"] "NestedOrdinaryPattern.hs"
        [(13, selected "NestedOrdinaryPattern.choose: unknown executable method/default dependency")]
    rejects "NestedCallableType.hs" [(14, tval), (19, tval)]
    checksNestedInstanceRoles
    putStrLn "PASS: generated-construction / nominal observer support and standalone demand shrinkers, checked ordinary computations / concrete selected methods, complete home dependencies and checked structural views / pre-caller guard"
  where
    specimen name = getDataFileName ("test" </> "generated-construction" </> name)
    tval = "Memorist.Base.Thunk.tval"
    eval1 = "Memorist.Base.Core.eval1"
    forbidden op = "unapproved runtime operation " ++ op ++ " is forbidden in generated code"
    external op = "unapproved external operation " ++ op ++ " is forbidden in generated code"
    supportUse op = "support utility used by ordinary computation: " ++ op
    qcValue name = "QuickCheck value used outside a generator or property: " ++ name
    wrappedForce wrapper = qcValue ("OperandPropertyLaws." ++ wrapper) ++ " (operand in ordinary callee OperandPropertyLaws.strict at this instantiation)"
    selected op = "unapproved selected method " ++ op
    unknown op = selected (op ++ ": unknown executable method/default dependency")
    reprFailure = selected "Memorist.Data.Interface.Data.reprM: reprM requires a concrete method-specific construction closure and audited carrier"
    functorWrapper = selected "GHC.Internal.Data.Functor.<$>: requires the concrete audited Memorist instance"
    carrier op = selected ("GHC.Internal.Base." ++ op ++ ": requires the concrete audited Memorist instance")
    protectedEq = selected "GHC.Classes.==: protected thunk/annotation/accounting operand"
    demandEq = selected "GHC.Classes.==: demand-data methods have no observation exemption"
    demandProducer = [InstanceMethod "Demand" ["Producer"] method | method <- ["fullDemand", "usage"]]
    view op = "unchecked structural view use/escape: " ++ op
    sized = external "Test.QuickCheck.Gen.sized"
    oneof = external "Test.QuickCheck.Gen.oneof"
    rejectsOption reason output = do
        typechecks output
        problems <- rejection output
        let expected = output ++ ": unsupported construction compiler options: " ++ reason
        unless (expected `elem` lines problems) (fail ("missing " ++ expected ++ "\nGot: " ++ problems))
    checksImplicitForcing = do
        let name = "QuickCheckImplicitForcing.hs"
        accepts name
        source <- specimen name >>= readFile
        let check mode pragma = withSystemTempDirectory ("construction-" ++ mode ++ "-") $ \directory -> do
                let output = directory </> name
                writeFile output ("{-# LANGUAGE " ++ pragma ++ " #-}\n" ++ source)
                rejectsOption ("implicit forcing (" ++ mode ++ ")") output
        -- Isolate argument forcing from the strict fields normally implied by Strict.
        check "Strict" "Strict, NoStrictData"
        check "StrictData" "StrictData"
    checksOperandPropertyCall = do
        output <- specimen "OperandPropertyCall.hs"
        typechecks output
        diagnostics <- rejection output
        -- Only the ordinary callee is at fault; the same property observes a
        -- thunk and constructs its generator on the very same source line.
        onlySite output (12, qcValue "OperandPropertyCall.strict") diagnostics
    checksFamilyOperands = do
        output <- specimen "OperandFamilyCalls.hs"
        upstream <- specimen "OperandFamilies.hs"
        typechecks output
        reports <- rejection output
        requireAt reports output [(13, qcValue "OperandFamilies.h"), (14, qcValue "OperandFamilies.law"),
            (15, qcValue "OperandFamilies.forceOpen"), (23, qcValue "OperandFamilies.generic"),
            (24, qcValue "OperandFamilies.strip"), (27, qcValue "OperandFamilies.routed"),
            (30, payloadForce "payloadLaw"), (33, payloadForce "recursiveLaw")]
        requireAt reports upstream [(24, forbidden tval)]
        cleanDefinitions reports output (map TopBinding ["draw", "prop_scalar", "local"])
      where payloadForce wrapper = qcValue ("OperandFamilies." ++ wrapper) ++ " (operand in ordinary callee OperandFamilies.forcePayload at this instantiation)"
    checksNestedInstanceRoles = do
        output <- specimen "NestedInstanceRoles.hs"
        typechecks output
        diagnostics <- rejection output
        let make tys = InstanceMethod "Make" tys "make"
            gen = make ["Gen", "Int"]
        checkDefinitions diagnostics output
            [(SoleRhs gen, []), (LocalBinding gen "deadShrink", [forbidden tval]),
             (make ["Bool"], [forbidden tval]), (make ["Property"], [])]
    checksMappedBang = do
        output <- specimen "QuickCheckMappedBang.hs"
        typechecks output
        parsed <- parseHaskellModule output
        reports <- rejection output >>= siteReports "OrdinaryBang.hs"
        case [at | pat@(L _ BangPat{}) <- collectAst parsed :: [LPat GhcPs], RealSrcSpan at _ <- [getLocA pat]] of
            [at] -> unless ((at, "strict pattern forces a QuickCheck value outside a generator or property") `elem` reports) $
                fail (pretty at ++ ": missing rejection at the remapped bang")
            _ -> fail (output ++ ": expected exactly one bang pattern")
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
             (51, unknown "QuickCheckRoles.make"),
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
    checksGeneratedDemandMethods = do
        (directory, _, output) <- completeArtifact "anf-demand methods" ("anf-demand" </> "Example.hs")
        diagnostics <- constructionReports output
        writeFile (directory </> "construction-reports.txt") (unlines diagnostics)
        -- These are actual generated dictionaries, not a model of the emitter.
        -- Neither observers nor standalone shrinkers are computations. Unrelated
        -- selected-method blockers are still inspected in this complete output.
        unless (not (null diagnostics) && any (isInfixOf "Generated.foldrT") diagnostics) $
            fail ("anf-demand must retain unrelated construction rejections:\n" ++ unlines diagnostics)
        when (any (isInfixOf "liftShrink") diagnostics) $
            fail ("anf-demand shrinker helpers must have support roles:\n" ++ unlines diagnostics)
        let pureMethod report = any (`isInfixOf` report)
                ["unapproved selected method GHC.Classes.==:", "unapproved selected method GHC.Classes.$dm/=:",
                 "unapproved selected method GHC.Internal.Show.show:", "unapproved selected method GHC.Internal.Show.showsPrec:",
                 "unapproved selected method GHC.Internal.Show.$dmshow:", "unapproved selected method GHC.Internal.Show.$dmshowList:"]
        when (any pureMethod diagnostics) $
            fail ("anf-demand pure demand Eq/Show must have selected method certificates:\n" ++ unlines (filter pureMethod diagnostics))
    checksGeneratedCallerArithmetic = do
        temporary <- getTemporaryDirectory
        directory <- createTempDirectory temporary "caller-arithmetic-artifact-"
        input <- specimen "CallerArithmeticSource.hs"
        copyFile input (directory </> "Example.hs")
        let output = directory </> "Generated.hs"
        result <- translateFile (directory </> "Example.hs") output
        either fail pure result
        verdict <- try @IOException (checkGeneratedConstruction output)
        let diagnostics = either ioeGetErrorString (const "") verdict
        unless (null diagnostics || (output ++ ":") `isPrefixOf` diagnostics) $
            fail ("unexpected checker failure for twiceM: " ++ diagnostics)
        writeFile (directory </> "construction-reports.txt") diagnostics
        -- Inspect the entire published graph, without stripping support bodies.
        -- Independent #8/#10 support reports may remain on this branch.
        cleanDefinitions diagnostics output [TopBinding "twiceM"]
        signature <- withLoadedGenerated output $ \summary -> do
            void (parseModule summary >>= typecheckModule)
            name <- resolveExport (ms_mod summary) "twiceM"
            Just (AnId binder) <- lookupName name
            pure (idType binder)
        case tcSplitSigmaTy signature of
            ([a], [predicate], _) | Just (cls, [argument]) <- getClassPredTys_maybe predicate,
                getName cls == numClassName, getTyVar_maybe argument == Just a -> pure ()
            _ -> fail ("twiceM must retain only Num a, found " ++ pretty signature)
        putStrLn ("PASS: caller arithmetic / complete generated twiceM graph, unchanged Num a / " ++ directory)
    checkIntegralContext fixture = do
        let label = takeBaseName fixture
        (_, _, output) <- completeArtifact label fixture
        signature <- withLoadedGenerated output $ \summary -> do
            checked <- parseModule summary >>= typecheckModule
            Just (group, _, _, _, _) <- pure (tm_renamed_source checked)
            core <- findModule (mkModuleName "Memorist.Base.Core") (Just (mkFastString "MemoristANT"))
            evaluators <- mapM (resolveValue core) ["eval", "eval1", "value"]
            case filter (`elem` collectAst group) evaluators of
                [] -> pure ()
                found -> fail (output ++ ": native sharing must not reference the runtime evaluator: " ++ unwords (map pretty found))
            name <- resolveExport (ms_mod summary) "viewsM"
            Just (AnId binder) <- lookupName name
            pure (idType binder)
        case tcSplitSigmaTy signature of
            ([a], [predicate], _) | Just (cls, [argument]) <- getClassPredTys_maybe predicate,
                getName cls == integralClassName, getTyVar_maybe argument == Just a ->
                    putStrLn ("PASS: " ++ label ++ " / exported viewsM requires only Integral a")
            _ -> fail (output ++ ": exported viewsM must require only Integral a; found " ++ pretty signature)
    checksPhantomProducer = do
        (directory, input, output) <- completeArtifact "phantom producer" ("polymorphic-phantom-producer" </> "Example.hs")
        diagnostics <- constructionReports output
        withLoadedGenerated output $ \summary ->
            parseModule summary >>= typecheckModule >>= checkPhantomAllocation
        cleanDefinitions (unlines diagnostics) output (map TopBinding ["viewsM", "inferredM", "makeM", "workM", "mapThunk"])
        mutant <- createTempDirectory directory "discarded-pair-"
        copyFile input (mutant </> "Example.hs")
        text <- readFile output
        let original = "in pure (M.PairT shared shared)"
            replacement = "in do { _ <- pure (M.PairT shared shared); first <- M.lazy (M.force shared); second <- M.lazy (M.force shared); pure (M.PairT first second) }"
            mutate line = case stripPrefix original (dropWhile (== ' ') line) of
                Just suffix -> takeWhile (== ' ') line ++ replacement ++ suffix
                Nothing -> line
            changed = map mutate (lines text)
            broken = mutant </> "Generated.hs"
        unless (length (filter id (zipWith (/=) (lines text) changed)) == 2) $
            fail "phantom allocation mutant must replace both returned shared pairs"
        writeFile broken (unlines changed)
        typechecks broken
        expectAssertionFailure "viewsM must suspend one producer invocation" $
            withLoadedGenerated broken $ \summary ->
                parseModule summary >>= typecheckModule >>= checkPhantomAllocation
        putStrLn "PASS: phantom producer / one suspended invocation, checked view, original constructor and fields"
    checksCompleteBox = do
        (directory, input, output) <- completeArtifact "Box" ("datatype-only-conversion" </> "Example.hs")
        definitions <- definitionSpans <$> parseHaskellModule output
        checkBox output definitions
        -- Split the abstract equation after its binder, preserving the columns
        -- of its remaining tokens. Support authority follows the actual body.
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
            [(InstanceMethod "Repr" ["Box"] "abstract", []),
              (InstanceMethod "Demand" ["BoxT"] "fullDemand", []),
              (InstanceMethod "Demand" ["BoxT"] "usage", []),
              (TopBinding "shrinkField", [])]
        mapM_ (\key -> check (key, []))
            [TopBinding "arbitraryField", TopBinding "genField", TopBinding "genBoxDemand",
              InstanceMethod "GeneratedDemand" ["BoxT"] "genDemandBudget", InstanceMethod "Arbitrary" ["BoxD"] "arbitrary",
              InstanceMethod "DemandArb" ["BoxT"] "arbDemandFrom", TopBinding "shrinkBoxDemand",
              InstanceMethod "GeneratedDemand" ["BoxT"] "shrinkDemandCompatible"]
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
    onlySite path target diagnostics = do
        reports <- siteReports path diagnostics
        unless (length reports == 1 && hasSite target reports) $
            fail (path ++ ": expected only " ++ show target ++ "\nGot: " ++ diagnostics)
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
             "-outputdir", objects, "-package", "MemoristANT", "-package", quickCheckPackage,
             "-package", "ghc-prim", output] ""
        unless (status == ExitSuccess) (fail (output ++ ": specimen must typecheck independently before its verdict\n" ++ out ++ err))

data SpecimenDefinition = TopBinding String | LocalBinding SpecimenDefinition String
    | InstanceMethod String [String] String | SoleRhs SpecimenDefinition deriving Eq

type BindingSpans = [(SpecimenDefinition, SrcSpan, SrcSpan)] -- selected region, then binder
type SiteReport = (RealSrcSpan, String)

instance Show SpecimenDefinition where
    show (TopBinding name) = name
    show (LocalBinding owner name) = name ++ " (in " ++ show owner ++ ")"
    show (SoleRhs owner) = "right-hand side of " ++ show owner
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
definitionSpans parsed = concat $
    [regions (TopBinding (occ name)) (getLocA decl) name matches
    | decl@(L _ (ValD _ (FunBind _ name matches))) <- hsmodDecls parsed] ++
    [regions (InstanceMethod (occ cls) tys (occ name)) (getLocA binding) name matches
    | L _ (InstD _ (ClsInstD _ inst)) <- hsmodDecls parsed,
      HsAppTy _ lhs rhs <- [unLoc (ignoreParens (getLHsInstDeclHead (cid_poly_ty inst)))],
      HsTyVar _ _ cls <- [unLoc (ignoreParens lhs)], Just tys <- [typeNames rhs],
      binding@(L _ (FunBind _ name matches)) <- toList (cid_binds inst)]
  where
    occ = occNameString . rdrNameOcc . unLoc
    regions key at name matches = (key, at, getLocA name) :
        [(LocalBinding key (occ n), getLocA binding, getLocA n)
        | binding@(L _ (FunBind _ n _)) <- collectAst matches :: [LHsBind GhcPs]] ++
        -- The expression span excludes the equation's where declarations.
        [(SoleRhs key, getLocA rhs, getLocA name)
        | [L _ match] <- [unLoc (mg_alts matches)], [L _ (GRHS _ _ rhs)] <- [grhssGRHSs (m_grhss match)]]
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
