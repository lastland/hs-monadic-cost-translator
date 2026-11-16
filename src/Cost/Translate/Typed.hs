module Cost.Translate.Typed (expressionTypes, hasFreeVariables, hasFreshAnnotationVariables) where

import GHC hiding (exprType)
import GHC.Core.Utils (exprType)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Types.Var.Set (isEmptyVarSet)
import GHC.HsToCore.Expr (dsLExpr)
import GHC.HsToCore.Monad (initDs)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

expressionTypes :: HscEnv -> TypecheckedModule -> [LHsExpr GhcRn] -> [LHsExpr GhcTc] -> IO (Map.Map String Type)
expressionTypes session checked source typed = do
    let locations = Set.fromList [location e | e <- source, interesting (unLoc e)]
        selected = Map.fromListWith (\_ outer -> outer)
            [(location e,e) | e <- typed, location e `Set.member` locations]
    (_, result) <- initDs session (fst (tm_internals_ checked)) $
        traverse (fmap exprType . dsLExpr) selected
    maybe (fail "GHC type-evidence desugaring failed") pure result
  where
    location = showSDocUnsafe . ppr . getLocA
    interesting HsVar {} = True
    interesting ExprWithTySig {} = True
    interesting _ = False

hasFreeVariables :: Type -> Bool
hasFreeVariables = not . isEmptyVarSet . tyCoVarsOfType

hasFreshAnnotationVariables :: LHsSigWcType GhcRn -> Bool
hasFreshAnnotationVariables = not . null . hsOuterTyVarNames . sig_bndrs . unLoc . hswc_body
