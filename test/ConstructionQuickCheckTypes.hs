-- | Identify the outer QuickCheck result by nominal type evidence in the
-- module that checked it. A downstream instance is not a declaration role.
module ConstructionQuickCheckTypes (quickCheckPackage, quickCheckResult, checkedHome) where

import ConstructionRuntime (resolveType)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import GHC
import GHC.Core.Coercion (Role(Nominal))
import GHC.Core.FamInstEnv (FamInstEnvs, emptyFamInstEnv, reduceTyFamApp_maybe, unionFamInstEnv)
import GHC.Core.Reduction (reductionReducedType)
import GHC.Core.TyCo.Rep (typeSize)
import GHC.Core.Type (splitPiTys, splitTyConApp_maybe)
import GHC.Data.FastString (mkFastString)
import GHC.Driver.Env (hscEPS)
import GHC.Tc.Types (tcg_fam_inst_env, tcg_imports, tcg_mod)
import GHC.Unit.External (eps_mod_fam_inst_env)
import GHC.Unit.Module.Deps (imp_finsts)
import GHC.Unit.Module.Env (lookupModuleEnv)

quickCheckPackage :: String
quickCheckPackage = "QuickCheck"

checkedHome :: TypecheckedModule -> Module
checkedHome = tcg_mod . fst . tm_internals_

quickCheckResult :: [TypecheckedModule] -> Ghc (Module -> Type -> Bool)
quickCheckResult modules = do
    qc <- findModule (mkModuleName "Test.QuickCheck") (Just (mkFastString quickCheckPackage))
    constructors <- mapM (resolveType qc) ["Gen", "Property"]
    packages <- eps_mod_fam_inst_env <$> (getSession >>= liftIO . hscEPS)
    let scope m = (foldr unionFamInstEnv emptyFamInstEnv imported, tcg_fam_inst_env global)
          where
            global = fst (tm_internals_ m)
            imported = mapMaybe (lookupModuleEnv packages) (imp_finsts (tcg_imports global))
        scopes = Map.fromList [(checkedHome m, scope m) | m <- modules]
        empty = (emptyFamInstEnv, emptyFamInstEnv)
    pure $ \home -> let env = Map.findWithDefault empty home scopes
                    in resultHead constructors env . snd . splitPiTys

-- Reduce only the result head, never newtype representations, dictionary
-- givens or a property's payload. GHC selects a checked nominal equation.
-- A family can recur only on a strictly smaller application along this path;
-- even a valid artifact can mention an unreducible or growing open-ended type.
resultHead :: [TyCon] -> FamInstEnvs -> Type -> Bool
resultHead constructors env = go Map.empty
  where
    go seen ty = case splitTyConApp_maybe ty of
        Just (tc, args)
            | tc `elem` constructors -> True
            | isTypeFamilyTyCon tc, length args >= tyConArity tc,
              let name = getName tc; size = typeSize ty,
              maybe True (size <) (Map.lookup name seen),
              Just step <- reduceTyFamApp_maybe env Nominal tc args ->
                go (Map.insert name size seen) (reductionReducedType step)
        _ -> False
