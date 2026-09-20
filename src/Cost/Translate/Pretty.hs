module Cost.Translate.Pretty
    ( Section(..), prettyModule, generatedPragma, renderModule
    , basicExtensions, polymorphicExtensions, reflectionExtensions, requiredExtensions
    ) where

import GHC.Driver.Session (DynFlags, xopt_set)
import qualified GHC.LanguageExtensions.Type as Extension
import GHC.Utils.Outputable (ppr, vcat)
import GHC.Driver.Ppr (showSDoc)
import Cost.Translate.Diagnostic (Diagnostic(..))
import Cost.Translate.Output (Decl)
import GHC (HsModule(..), GhcPs, unLoc, moduleNameString)
import Data.Char (isUpper, isAlphaNum)

data Section = Section
    { sectionCommon :: Bool
    , sectionName :: String
    , sectionDeclarations :: [Decl]
    }

renderModule :: DynFlags -> HsModule GhcPs -> String
renderModule flags ast = renderPreamble generatedExtensions ++ showSDoc renderedFlags (ppr ast) ++ "\n"
  where renderedFlags = foldl xopt_set flags generatedExtensions

renderSectionedModule :: DynFlags -> [Extension.Extension] -> HsModule GhcPs -> [Section] -> String
renderSectionedModule flags extensions ast sections = renderPreamble extensions ++
    showSDoc renderedFlags (ppr (ast { hsmodDecls = [] })) ++
    concatMap (renderSection renderedFlags) sections ++ "\n"
  where
    renderedFlags = foldl xopt_set flags generatedExtensions
    renderSection sectionFlags (Section common name declarations) =
        "\n-- ==== " ++ (if common then "Common: " else "Module-specific: ") ++ name ++ " ====\n\n" ++
        showSDoc sectionFlags (vcat (map ppr declarations)) ++ "\n"

renderPreamble :: [Extension.Extension] -> String
renderPreamble extensions = unlines (filter (not . null) [generatedPragma extensions, generatedOptions])

-- The only output rendering pass. GHC syntax is already structural; there is
-- no reparse of assembled source. ModuleName itself accepts arbitrary strings,
-- so validate that token before publication (including the CLI's bad-name case).
prettyModule :: DynFlags -> [Extension.Extension] -> HsModule GhcPs -> [Section] -> Either Diagnostic String
prettyModule flags extensions ast sections = if maybe False (validModule . moduleNameString . unLoc) (hsmodName ast)
    then Right code else Left (GeneratedModuleNameInvalid code)
  where code = renderSectionedModule flags extensions ast sections

validModule :: String -> Bool
validModule name = case break (== '.') name of
    (first:rest, suffix) -> isUpper first && all (\c -> isAlphaNum c || c `elem` "_'") rest &&
        case suffix of { [] -> True; _:more -> validModule more }
    _ -> False

generatedExtensions :: [Extension.Extension]
generatedExtensions =
    [ Extension.RankNTypes, Extension.TypeFamilies, Extension.FlexibleInstances
    , Extension.FlexibleContexts, Extension.UndecidableInstances
    , Extension.UndecidableSuperClasses, Extension.PolyKinds, Extension.ConstraintKinds
    , Extension.QuantifiedConstraints, Extension.StandaloneKindSignatures
    , Extension.StandaloneDeriving, Extension.ScopedTypeVariables, Extension.DataKinds, Extension.TypeOperators
    , Extension.MagicHash
    ]

generatedPragma :: [Extension.Extension] -> String
generatedPragma extensions = if null unique then "" else
    "{-# LANGUAGE " ++ concatMap ((++ ", ") . show) unique ++ "NoMonoLocalBinds #-}"
  where unique = foldr add [] extensions
        add extension seen = if extension `elem` seen then seen else extension : seen

basicExtensions :: [Extension.Extension]
basicExtensions = [Extension.TypeFamilies, Extension.FlexibleInstances, Extension.FlexibleContexts, Extension.UndecidableInstances]

polymorphicExtensions :: [Extension.Extension]
polymorphicExtensions = basicExtensions ++ [Extension.RankNTypes, Extension.KindSignatures]

reflectionExtensions :: [Extension.Extension]
reflectionExtensions = polymorphicExtensions ++
    [Extension.PolyKinds, Extension.StandaloneKindSignatures, Extension.ScopedTypeVariables, Extension.DataKinds, Extension.TypeOperators]

requiredExtensions :: Bool -> [Extension.Extension]
requiredExtensions magicHash = if magicHash then generatedExtensions else filter (/= Extension.MagicHash) generatedExtensions

generatedOptions :: String
generatedOptions = unlines
    [ "-- Runtime-owned types require local instances; generic dictionaries retain intentional constraints."
    , "{-# OPTIONS_GHC -Wno-orphans -Wno-simplifiable-class-constraints #-}"
    ]
