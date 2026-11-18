module Cost.Translate.Frontend.Module (emitModule) where

import Cost.Translate.Datatypes (emitDatatypeSections)
import Cost.Translate.Frontend.Emit (emitBind)
import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.Gates (checkRecursiveValues)
import Cost.Translate.Frontend.Logs (moduleLogs)
import Cost.Translate.Frontend.ModulePlan
import Cost.Translate.Frontend.NamePlan (planNames)
import Cost.Translate.Frontend.Sections
import Cost.Translate.Frontend.Syntax (arity)
import Cost.Translate.Names (generatedName)
import Cost.Translate.Output qualified as O
import Cost.Translate.Pretty (Section)
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
    Map.Map String String ->
    Map.Map String Type ->
    Map.Map Name Typed.FunctionType ->
    HsGroup GhcRn ->
    (Either String (HsModule GhcPs, [Section], [Extension.Extension]), String)
emitModule strict target source hash dts original literals hints checked group =
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
        (helperTypes, adjustType) = specializeHelpers group checked hints
        types = helperTypes `Map.union` Map.map adjustType original
        adjustedHints = Map.map adjustType hints
        checkedScopes =
            Map.mapWithKey
                ( \name info ->
                    ( [ variable
                      | name `Map.notMember` helperTypes
                      , variable <- Typed.functionVariables info
                      ]
                    , map adjustType (Typed.functionGivens info)
                    )
                )
                checked
        (constructorEnv, specialized) =
            moduleEnvironment
                strict
                dts
                literals
                types
                adjustedHints
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
    Map.Map Name [Type] ->
    HsGroup GhcRn ->
    Either String (HsModule GhcPs, [Section], [Extension.Extension])
moduleResult target source hash helpers env specialized group = do
    mapM_ checkRecursiveValues (topLevelGroups group)
    (datatypeSupport, kindSupport, datatypeDeclarations) <-
        emitDatatypeSections datatypes
    declarations <-
        concat <$> mapM emitBinding (concatMap snd (topLevelGroups group))
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
        datatypes = sourceDatatypes env
        reserved = reservedModuleNames datatypes group
        layouts = constructorLayouts datatypes
        emitBinding binding = do
            (name, ty) <- bindingSignature env binding
            let plan = planNames reserved layouts specialized binding
            code <- emitBind (env {namePlan = plan}) binding
            pure [O.sig name ty, code]
