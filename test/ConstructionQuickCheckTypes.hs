-- | Identify the outer QuickCheck result by nominal type evidence in the
-- module that checked it. A downstream instance is not a declaration role.
module ConstructionQuickCheckTypes (quickCheckPackage, quickCheckResult, checkedHome) where

import ConstructionAst (collectAst)
import ConstructionRuntime (resolveType)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import GHC
import GHC.Core.Coercion (Role(Nominal), coercionLKind, coercionRKind, isReflCo)
import GHC.Core.FamInstEnv (FamInstEnvs, emptyFamInstEnv, reduceTyFamApp_maybe, unionFamInstEnv)
import GHC.Core.Reduction (reductionReducedType)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.FVs (tyCoVarsOfCo)
import GHC.Core.TyCo.Rep (Coercion(..), typeSize)
import GHC.Core.Type (splitPiTys, splitTyConApp_maybe)
import GHC.Data.FastString (mkFastString)
import GHC.Driver.Env (hscEPS)
import GHC.Tc.Types (tcg_fam_inst_env, tcg_imports, tcg_mod)
import GHC.Tc.Types.Evidence (HsWrapper(..))
import GHC.Types.Var.Set (isEmptyVarSet)
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
    let inModule m = resultHead constructors env (familyWitnesses constructors env m)
          where
            global = fst (tm_internals_ m)
            imported = mapMaybe (lookupModuleEnv packages) (imp_finsts (tcg_imports global))
            env = (foldr unionFamInstEnv emptyFamInstEnv imported, tcg_fam_inst_env global)
        scopes = Map.fromList [(checkedHome m, inModule m) | m <- modules]
        external = resultHead constructors (emptyFamInstEnv, emptyFamInstEnv) []
    pure $ \home -> Map.findWithDefault external home scopes . snd . splitPiTys

familyStep :: FamInstEnvs -> TyCon -> [Type] -> Maybe Type
familyStep env tc args
    | isTypeFamilyTyCon tc, length args >= tyConArity tc =
        reductionReducedType <$> reduceTyFamApp_maybe env Nominal tc args
    | otherwise = Nothing

-- A checked cast can already contain a whole finite reduction, including
-- steps under an argument. Use its exact closed endpoints, not a general
-- equation or a caller's given. Replay axiom leaves in this module's scope.
familyWitnesses :: [TyCon] -> FamInstEnvs -> TypecheckedModule -> [Type]
familyWitnesses constructors env m =
    [ty | w <- collectAst (tm_typechecked_source m), co <- casts w, certified co,
        isEmptyVarSet (tyCoVarsOfCo co), let left = coercionLKind co; right = coercionRKind co,
        (ty, result) <- [(left, right), (right, left)], Just (tc, _) <- [splitTyConApp_maybe ty], isTypeFamilyTyCon tc,
        Just (actual, _) <- [splitTyConApp_maybe result], actual `elem` constructors]
  where
    casts (WpCast co) = [co]
    casts (WpCompose a b) = casts a ++ casts b
    casts _ = []
    certified (SubCo c) = certified c
    certified (SymCo c) = certified c
    certified (TransCo a b) = certified a && certified b
    certified (TyConAppCo Nominal _ cs) = all certified cs
    certified (AppCo a b) = certified a && certified b
    certified c@(AxiomInstCo _ _ args) | all isReflCo args,
        Just (tc, ts) <- splitTyConApp_maybe (coercionLKind c) =
            maybe False (eqType (coercionRKind c)) (familyStep env tc ts)
    certified c = isReflCo c

-- Reduce only the result head, never newtype representations, dictionary
-- givens or a property's payload. GHC selects a checked nominal equation.
-- A family can recur only on a strictly smaller application along this path;
-- even a valid artifact can mention an unreducible or growing open-ended type.
resultHead :: [TyCon] -> FamInstEnvs -> [Type] -> Type -> Bool
resultHead constructors env witnessed = go Map.empty
  where
    go seen ty = case splitTyConApp_maybe ty of
        Just (tc, args)
            | tc `elem` constructors -> True
            | isTypeFamilyTyCon tc ->
                let name = getName tc; size = typeSize ty
                    reduces = maybe True (size <) (Map.lookup name seen) &&
                        maybe False (go (Map.insert name size seen)) (familyStep env tc args)
                in reduces || any (eqType ty) witnessed
        _ -> False
