module Cost.Translate.Frontend.Sections
    ( moduleSections
    , moduleImports
    , moduleExtensions
    ) where

import Cost.Translate.Foldable (foldableSupport)
import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.NamePlan (NamePlan (..))
import Cost.Translate.Frontend.Syntax (collect)
import Cost.Translate.Output qualified as O
import Cost.Translate.Pretty
    ( Section (..)
    , basicExtensions
    , polymorphicExtensions
    , reflectionExtensions
    , requiredExtensions
    )
import Cost.Translate.Sharing (nativeSharingSupport, sharingSupport)
import Cost.Translate.Sharing.Schema (nativeSchemaSupport)
import Cost.Translate.Validation (scalarSupport)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC
import GHC.Core.Type (splitForAllTyVars)
import GHC.LanguageExtensions.Type qualified as Extension
import GHC.Types.Name.Occurrence (occNameString)

moduleSections ::
    String -> Env -> [O.Decl] -> [O.Decl] -> [O.Decl] -> [O.Decl] -> [Section]
moduleSections source env datatypeSupport kindSupport datatypes declarations =
    [ Section
        True
        "sharing and scalar support"
        ( concat [sharingSupport reflected | needsSharing]
            ++ concat [nativeSharingSupport listMapping | native]
            ++ schemaSupport
            ++ scalarSupport
        )
    , Section True "foldable support" foldableSupport
    , Section True "datatype conversion and demand support" datatypeSupport
    , Section
        False
        ("source datatypes and conversions from " ++ source)
        (kindSupport ++ datatypes)
    , Section False ("translated bindings from " ++ source) declarations
    ]
    where
        schemaSupport =
            nativeSchemaSupport
                (plannedNativeSchemas (namePlan env))
                (supportReferences declarations)
        references = supportReferences (schemaSupport ++ declarations)
        reflected = not (Set.null (reflectiveBindings env))
        listMapping = "mapListT" `Set.member` references
        needsSharing = any (`Set.member` references) ["sharedAt", "sharedAtBy"]
        native = "mapThunk" `Set.member` references

supportReferences :: [O.Decl] -> Set.Set String
supportReferences declarations =
    Set.fromList
        [ occNameString occ
        | HsVar _ (L _ (Unqual occ)) <- collect declarations :: [HsExpr GhcPs]
        ]

moduleExtensions ::
    Bool -> Bool -> Bool -> Env -> [O.Decl] -> [Extension.Extension]
moduleExtensions magicHash specialized helpers env declarations =
    [Extension.MagicHash | magicHash]
        ++ [Extension.LiberalTypeSynonyms | not (Map.null (producerCalls env))]
        ++ [Extension.ImpredicativeTypes | applications]
        ++ [Extension.TypeApplications | applications]
        ++ [ Extension.ScopedTypeVariables
           | native || specialized || helpers
           ]
        ++ enabled
    where
        expressions = collect declarations :: [HsExpr GhcPs]
        applications = not (null [() | HsAppType {} <- expressions])
        native = "mapThunk" `Set.member` supportReferences declarations
        types = Map.elems (inferredBindings env)
        enabled
            | not (null (sourceDatatypes env)) = requiredExtensions False
            | not (Set.null (reflectiveBindings env)) = reflectionExtensions
            | not (all (null . fst . splitForAllTyVars) types) =
                polymorphicExtensions
            | otherwise = basicExtensions

moduleImports ::
    String -> Set.Set String -> [Section] -> [LImportDecl GhcPs]
moduleImports source reserved sections =
    [ if null hidden
        then O.importD "Prelude" Nothing
        else O.hideImport "Prelude" hidden
    ]
        ++ [O.importD "Data.Kind" Nothing | "Type" `Set.member` unqualified]
        ++ [O.importD "Data.Proxy" Nothing | "Proxy" `Set.member` unqualified]
        ++ concat
            [ qualifiedImport "Memorist.Memorist" "M"
            , qualifiedImport "GHC.TypeLits" "TypeLits"
            , qualifiedImport "Type.Reflection" "Reflection"
            , qualifiedImport "Data.List.NonEmpty" "NE"
            , qualifiedImport "Data.Char" "Char"
            , qualifiedImport source "Source"
            , qualifiedImport "Test.QuickCheck" "QC"
            , qualifiedImport "Memorist.Example.List" "ML"
            , qualifiedImport "Memorist.Example.ListInf" "MI"
            , qualifiedImport "Memorist.Example.NewtonRaphson" "MN"
            , qualifiedImport "Memorist.Data.List" "L"
            , qualifiedImport "Data.Void" "Void"
            ]
    where
        names = collect (concatMap sectionDeclarations sections) :: [RdrName]
        usedQualifiers = Set.fromList [moduleNameString q | Qual q _ <- names]
        unqualified = Set.fromList [occNameString o | Unqual o <- names]
        hidden = Set.toList (Set.filter isPreludeExport reserved)
        qualifiedImport name alias =
            [O.importD name (Just alias) | alias `Set.member` usedQualifiers]

isPreludeExport :: String -> Bool
isPreludeExport name =
    name
        `elem` [ "mapM"
               , "mapM_"
               , "sequence"
               , "sequence_"
               , "foldr"
               , "foldl"
               , "filter"
               , "zipWith"
               , "concatMap"
               ]
