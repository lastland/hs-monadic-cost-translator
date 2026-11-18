module Cost.Translate.Pretty
    ( Section (..)
    , prettyModule
    , generatedPragma
    , renderModule
    , basicExtensions
    , polymorphicExtensions
    , reflectionExtensions
    , requiredExtensions
    ) where

import Cost.Translate.Diagnostic (Diagnostic (..))
import Cost.Translate.Output (Decl)
import Data.Char (isAlphaNum, isUpper)
import Data.List (intercalate, intersect, partition, sortOn)
import GHC
    ( GhcPs
    , HsBindLR (..)
    , HsDecl (..)
    , HsModule (..)
    , ImportDecl (..)
    , ImportDeclQualifiedStyle (..)
    , LImportDecl
    , RdrName
    , Sig (..)
    , StandaloneKindSig (..)
    , moduleNameString
    , tcdName
    , unLoc
    )
import GHC.Driver.Ppr (showSDoc)
import GHC.Driver.Session (DynFlags, xopt_set)
import GHC.LanguageExtensions.Type qualified as Extension
import GHC.Utils.Outputable (SDoc, ppr, text, vcat)

data Section = Section
    { sectionCommon :: Bool
    , sectionName :: String
    , sectionDeclarations :: [Decl]
    }

renderModule :: DynFlags -> HsModule GhcPs -> String
renderModule flags ast = renderPreamble generatedExtensions ++ showSDoc renderedFlags body ++ "\n"
    where
        renderedFlags = foldl xopt_set flags generatedExtensions
        body = renderBody ast [renderDeclarations declarations | not (null declarations)]
        declarations = hsmodDecls ast

renderSectionedModule ::
    DynFlags -> [Extension.Extension] -> HsModule GhcPs -> [Section] -> String
renderSectionedModule flags extensions ast sections =
    renderPreamble extensions
        ++ showSDoc
            renderedFlags
            (renderBody ast [separated (map renderSection sections) | not (null sections)])
        ++ "\n"
    where
        renderedFlags = foldl xopt_set flags generatedExtensions
        renderSection (Section common name declarations) = separated (text banner : declarationBlock)
            where
                banner =
                    "-- ==== "
                        ++ (if common then "Common: " else "Module-specific: ")
                        ++ name
                        ++ " ===="
                declarationBlock = [renderDeclarations declarations | not (null declarations)]

renderBody :: HsModule GhcPs -> [SDoc] -> SDoc
renderBody ast declarations = separated (moduleLine : imports ++ declarations)
    where
        moduleLine = ppr (ast {hsmodImports = [], hsmodDecls = []})
        imports = renderImports (hsmodImports ast)

renderImports :: [LImportDecl GhcPs] -> [SDoc]
renderImports imports = map renderGroup (filter (not . null) [plain, qualified])
    where
        (plain, qualified) = partition ((== NotQualified) . ideclQualified . unLoc) imports
        renderGroup = vcat . map ppr . sortOn importName
        importName = moduleNameString . unLoc . ideclName . unLoc

renderDeclarations :: [Decl] -> SDoc
renderDeclarations = separated . map (vcat . map ppr) . declarationGroups

separated :: [SDoc] -> SDoc
separated = vcat . intercalate [text ""] . map (: [])

declarationGroups :: [Decl] -> [[Decl]]
declarationGroups = go []
    where
        go pending [] = map (: []) pending
        go pending (declaration : rest) = case signatureNames declaration of
            [] ->
                let (matching, remaining) = partition (matches declaration) pending
                 in (matching ++ [declaration]) : go remaining rest
            _ -> go (pending ++ [declaration]) rest
        matches declaration signature =
            not (null (signatureNames signature `intersect` declarationNames declaration))

signatureNames :: Decl -> [RdrName]
signatureNames located = case unLoc located of
    SigD _ (TypeSig _ names _) -> map unLoc names
    KindSigD _ (StandaloneKindSig _ name _) -> [unLoc name]
    _ -> []

declarationNames :: Decl -> [RdrName]
declarationNames located = case unLoc located of
    ValD _ FunBind {fun_id = name} -> [unLoc name]
    TyClD _ declaration -> [tcdName declaration]
    _ -> []

renderPreamble :: [Extension.Extension] -> String
renderPreamble extensions =
    intercalate
        "\n"
        (filter (not . null) [generatedPragma extensions, generatedOptions])
        ++ "\n\n"

prettyModule ::
    DynFlags ->
    [Extension.Extension] ->
    HsModule GhcPs ->
    [Section] ->
    Either Diagnostic String
prettyModule flags extensions ast sections =
    if maybe False (validModule . moduleNameString . unLoc) (hsmodName ast)
        then Right code
        else Left (GeneratedModuleNameInvalid code)
    where
        code = renderSectionedModule flags extensions ast sections

validModule :: String -> Bool
validModule name = case break (== '.') name of
    (first : rest, suffix) ->
        isUpper first
            && all (\c -> isAlphaNum c || c `elem` "_'") rest
            && case suffix of [] -> True; _ : more -> validModule more
    _ -> False

generatedExtensions :: [Extension.Extension]
generatedExtensions =
    [ Extension.RankNTypes
    , Extension.TypeFamilies
    , Extension.FlexibleInstances
    , Extension.FlexibleContexts
    , Extension.UndecidableInstances
    , Extension.UndecidableSuperClasses
    , Extension.PolyKinds
    , Extension.ConstraintKinds
    , Extension.QuantifiedConstraints
    , Extension.StandaloneKindSignatures
    , Extension.StandaloneDeriving
    , Extension.ScopedTypeVariables
    , Extension.DataKinds
    , Extension.TypeOperators
    , Extension.MagicHash
    ]

generatedPragma :: [Extension.Extension] -> String
generatedPragma extensions =
    if null unique
        then ""
        else
            "{-# LANGUAGE\n    "
                ++ intercalate ",\n    " (map show unique ++ ["NoMonoLocalBinds"])
                ++ " #-}"
    where
        unique = foldr add [] extensions
        add extension seen = if extension `elem` seen then seen else extension : seen

basicExtensions :: [Extension.Extension]
basicExtensions =
    [ Extension.TypeFamilies
    , Extension.FlexibleInstances
    , Extension.FlexibleContexts
    , Extension.UndecidableInstances
    ]

polymorphicExtensions :: [Extension.Extension]
polymorphicExtensions = basicExtensions ++ [Extension.RankNTypes, Extension.KindSignatures]

reflectionExtensions :: [Extension.Extension]
reflectionExtensions =
    polymorphicExtensions
        ++ [ Extension.PolyKinds
           , Extension.StandaloneKindSignatures
           , Extension.ScopedTypeVariables
           , Extension.DataKinds
           , Extension.TypeOperators
           ]

requiredExtensions :: Bool -> [Extension.Extension]
requiredExtensions magicHash =
    if magicHash
        then generatedExtensions
        else filter (/= Extension.MagicHash) generatedExtensions

generatedOptions :: String
generatedOptions =
    "-- Runtime-owned types require local instances; generic dictionaries retain intentional constraints.\n"
        ++ "{-# OPTIONS_GHC -Wno-orphans -Wno-simplifiable-class-constraints #-}"
