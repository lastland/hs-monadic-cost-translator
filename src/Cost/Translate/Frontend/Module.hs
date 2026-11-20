module Cost.Translate.Frontend.Module (emitModule) where

import Cost.Translate.Datatypes (emitDatatypeSections)
import Cost.Translate.Frontend.Emit (emitBind, renderPayload)
import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.Expression.Evidence (ExpressionEvidence (..))
import Cost.Translate.Frontend.Gates (checkRecursiveValues)
import Cost.Translate.Frontend.Logs (moduleLogs)
import Cost.Translate.Frontend.ModulePlan
import Cost.Translate.Frontend.NamePlan (planNames)
import Cost.Translate.Frontend.Sections
import Cost.Translate.Frontend.Syntax (arity)
import Cost.Translate.Names (generatedName)
import Cost.Translate.Output qualified as O
import Cost.Translate.Pretty (Section)
import Cost.Translate.Sharing (Specialization)
import Cost.Translate.Sharing.Producer (emitProducer, planProducers)
import Cost.Translate.Specialize (specializeHelpers)
import Cost.Translate.Typed qualified as Typed
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC
import GHC.LanguageExtensions.Type qualified as Extension

emitModule ::
    Set.Set Name ->
    String ->
    String ->
    Bool ->
    [TyCon] ->
    Map.Map Name Type ->
    Map.Map Typed.SourceKey String ->
    ExpressionEvidence ->
    Map.Map Name Typed.FunctionType ->
    HsGroup GhcRn ->
    (Either String (HsModule GhcPs, [Section], [Extension.Extension]), String)
emitModule strict target source hash dts raw literals evidence checked group =
    ( moduleResult
        target
        source
        hash
        (not (Map.null helperTypes))
        env
        specialized
        group
    , moduleLogs constructorEnv group
    )
    where
        hints = occurrenceTypes evidence
        (helperTypes, adjustType) = specializeHelpers group checked hints
        types = helperTypes `Map.union` Map.map adjustType raw
        adjustedHints = Map.map adjustType hints
        checkedScopes = Map.mapWithKey scope checked
        scope name info
            | name `Map.member` helperTypes =
                empty {Typed.checkedGivens = Typed.checkedGivens givens}
            | otherwise = givens
            where
                empty = Typed.emptyAllocationScope
                givens = Typed.functionGivens adjustType info
        (constructorEnv, specialized) =
            moduleEnvironment
                strict
                dts
                literals
                types
                evidence {occurrenceTypes = adjustedHints}
                checkedScopes
                group
        binds = concatMap snd (topLevelGroups group)
        env =
            extendEnv
                [ (unLoc n, Function (generatedName "M" (unLoc n)) (arity ms))
                | L _ (FunBind _ n ms) <- binds
                ]
                constructorEnv

moduleResult ::
    String ->
    String ->
    Bool ->
    Bool ->
    Env ->
    Map.Map Name [Specialization] ->
    HsGroup GhcRn ->
    Either String (HsModule GhcPs, [Section], [Extension.Extension])
moduleResult target source hash helpers initial specialized group = do
    mapM_ checkRecursiveValues groups
    (datatypeSupport, kindSupport, datatypeDeclarations) <-
        emitDatatypeSections datatypes
    declarations <-
        concat <$> mapM emitBinding (concatMap snd groups)
    let sections =
            moduleSections
                source
                env
                datatypeSupport
                kindSupport
                datatypeDeclarations
                declarations
        extensions =
            moduleExtensions
                hash
                (not (Map.null specialized))
                helpers
                env
                declarations
        imports = moduleImports source reserved sections
    pure (O.moduleD target imports [], sections, extensions)
    where
        groups = topLevelGroups group
        (workers, calls) = planProducers initial groups
        env = initial {producerCalls = calls}
        datatypes = sourceDatatypes env
        reserved =
            reservedModuleNames datatypes group `Set.union` workerNames
        workerNames = Set.fromList (Map.elems calls)
        layouts = constructorLayouts datatypes
        emitBinding binding = do
            (name, ty) <- bindingSignature env binding
            let plan = planNames datatypes reserved layouts specialized binding
            case unLoc binding of
                FunBind _ n _
                    | Just worker <- Map.lookup (unLoc n) workers ->
                        emitProducer renderPayload env name ty worker
                _ -> do
                    code <- emitBind (env {namePlan = plan}) binding
                    pure [O.sig name ty, code]
