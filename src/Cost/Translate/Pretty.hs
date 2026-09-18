module Cost.Translate.Pretty (prettyModule, generatedPragma, renderModule) where

import GHC.Driver.Session (DynFlags, xopt_set)
import qualified GHC.LanguageExtensions.Type as Extension
import GHC.Utils.Outputable (ppr)
import GHC.Driver.Ppr (showSDoc)
import Cost.Translate.Diagnostic (Diagnostic(..))
import GHC (HsModule(..), GhcPs, unLoc, moduleNameString)
import Data.Char (isUpper, isAlphaNum)

renderModule :: DynFlags -> HsModule GhcPs -> String
renderModule flags ast = generatedPragma ++ "\n" ++ showSDoc (foldl xopt_set flags generatedExtensions) (ppr ast) ++ "\n"

-- The only output rendering pass. GHC syntax is already structural; there is
-- no reparse of assembled source. ModuleName itself accepts arbitrary strings,
-- so validate that token before publication (including the CLI's bad-name case).
prettyModule :: DynFlags -> HsModule GhcPs -> Either Diagnostic String
prettyModule flags ast = if maybe False (validModule . moduleNameString . unLoc) (hsmodName ast)
    then Right code else Left (GeneratedModuleNameInvalid code)
  where code = renderModule flags ast

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

generatedPragma :: String
generatedPragma = "{-# LANGUAGE " ++ concatMap ((++ ", ") . show) generatedExtensions ++ "NoMonoLocalBinds #-}"
