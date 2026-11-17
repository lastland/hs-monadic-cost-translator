module Cost.Translate.Typed
    ( expressionTypes, FunctionType(..), functionTypes, scopedVariables
    , hasFreeVariables, hasFreshAnnotationVariables
    ) where

import GHC hiding (exprType)
import GHC.Core.Utils (exprType)
import GHC.Core.Type (mkVisFunTy)
import GHC.Core.TyCo.Rep (scaledMult, scaledThing)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Data.Bag (bagToList)
import GHC.Types.Var (Var)
import GHC.Types.Var.Set (elemVarSet, isEmptyVarSet)
import GHC.HsToCore.Expr (dsLExpr)
import GHC.HsToCore.Monad (initDs)
import GHC.Tc.Types.Evidence (HsWrapper(..))
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import Data.Data (Data, cast, gmapQ)
import Data.Maybe (isJust)
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

data FunctionType = FunctionType
    { functionDeclaredType :: Type
    , functionVariables :: [TyVar]
    , functionBodyType :: Type
    }

functionTypes :: TypecheckedModule -> Map.Map Name FunctionType
functionTypes checked = Map.mapMaybe unique $ Map.fromListWith (++)
    [(getName (abe_poly export), [FunctionType (idType (abe_poly export))
        (variables abstraction export wrapper) (bodyType (mg_ext matches))]) |
     abstraction <- checkedAbstractions (tm_typechecked_source checked),
     export <- abs_exports abstraction,
     L _ (FunBind (wrapper,_) identifier matches) <- bagToList (abs_binds abstraction),
     unLoc identifier == abe_mono export]
  where
    unique [function] = Just function
    unique _ = Nothing
    variables abstraction export wrapper
        | abs_sig abstraction = wrapperVariables wrapper
        | otherwise = [variable | variable <- abs_tvs abstraction,
            variable `elemVarSet` tyCoVarsOfType (idType (abe_mono export))]
    bodyType matches = foldr (\argument -> mkVisFunTy (scaledMult argument) (scaledThing argument))
        (mg_res_ty matches) (mg_arg_tys matches)

scopedVariables :: TypecheckedModule -> Map.Map Name [TyVar]
scopedVariables = Map.map functionVariables . functionTypes

checkedAbstractions :: Data a => a -> [AbsBinds]
checkedAbstractions value
    | Just abstraction <- cast value =
        abstraction : checkedAbstractions (abs_binds abstraction)
    | isJust (cast value :: Maybe Var)
      || isJust (cast value :: Maybe Type)
      || isJust (cast value :: Maybe HsWrapper) = []
    | otherwise = concat (gmapQ checkedAbstractions value)

wrapperVariables :: HsWrapper -> [TyVar]
wrapperVariables (WpCompose left right) = wrapperVariables left ++ wrapperVariables right
wrapperVariables (WpTyLam variable) = [variable]
wrapperVariables _ = []

hasFreeVariables :: Type -> Bool
hasFreeVariables = not . isEmptyVarSet . tyCoVarsOfType

hasFreshAnnotationVariables :: LHsSigWcType GhcRn -> Bool
hasFreshAnnotationVariables = not . null . hsOuterTyVarNames . sig_bndrs . unLoc . hswc_body
