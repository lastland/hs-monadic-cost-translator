-- | Link a context-free home Repr dictionary to its actual reprM implementation.
-- These are conditional certificates: every module body and retained source
-- site is checked, and every caller must supply the audited carrier.
module ConstructionHomeRepr (HomeReprs, homeReprs, selectedHome, methodCarrier) where

import ConstructionAst (collectAst)
import ConstructionRuntime
import Control.Monad (guard)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import GHC
import qualified GHC.Core as C
import GHC.Core.Class (classAllSelIds, classTyCon)
import GHC.Core.DataCon (dataConWorkId)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.Type (mkTyVarTy, splitForAllTyVars, substTyWith)
import GHC.Core.TyCon (tyConSingleDataCon)
import GHC.Tc.Types.Evidence (HsWrapper(..))
import GHC.Types.Id (isDFunId)
import GHC.Types.Var.Set (VarSet, emptyVarSet, mkVarSet, subVarSet)

data HomeReprs = HomeReprs (Map.Map Name ([TyVar], Type)) (Map.Map Name VarSet)

homeReprs :: Runtime -> [(TypecheckedModule, [C.CoreBind])] -> HomeReprs
homeReprs rt programs = HomeReprs (Map.fromList (map fst entries)) (Map.fromList (map snd entries))
  where
    entries = [((getName owner, scheme), (getName carrier, mkVarSet parameters)) | (checked, program) <- programs,
        let bindings = C.flattenBinds program, let sources = sourceFields rt checked,
        (owner, dictionary) <- bindings, Just (scheme, implementation) <- [homeField rt owner dictionary],
        (method, carrier) <- sources, implementation == method,
        Just (carrierClass, [_, _]) <- [getClassPredTys_maybe (idType carrier)],
        getName carrierClass == runtimeName rt "Memorist.Base.Core" "Memorist",
        let arity = length (fst scheme), Just rhs <- [lookup method bindings],
        let parameters = take arity (fst (C.collectTyBinders rhs)), length parameters == arity]

-- The DFun must construct the real dictionary, passing its type parameters
-- unchanged to the actual reprM field. Value/dictionary parameters do not fit.
homeField :: Runtime -> Id -> C.CoreExpr -> Maybe (([TyVar], Type), Id)
homeField rt owner dictionary = do
    guard (isDFunId owner && length variables == length actual)
    (cls, [ty]) <- getClassPredTys_maybe predicate
    guard (getName cls == runtimeName rt "Memorist.Data.Interface.Data" "Repr")
    let selectors = classAllSelIds cls
    (C.Var worker, C.Type headTy : fields) <- pure (C.collectArgs body)
    guard (worker == dataConWorkId (tyConSingleDataCon (classTyCon cls)))
    guard (eqType headTy (substTyWith variables actual ty) && length selectors == length fields)
    [field] <- pure [f | (sel, f) <- zip selectors fields, roleOf rt (getName sel) == Just ReprMethod]
    (C.Var method, supplied) <- pure (C.collectArgs field)
    guard (length supplied == length actual && and (zipWith sameType supplied actual))
    pure ((variables, ty), method)
  where
    (variables, predicate) = splitForAllTyVars (idType owner)
    (binders, body) = C.collectTyBinders dictionary
    actual = map mkTyVarTy binders
    sameType (C.Type t) = eqType t
    sameType _ = const False

-- Link the field back to its retained source binder as well. Inlining or
-- aliases must never turn an ordinary generic helper into a method body.
sourceFields :: Runtime -> TypecheckedModule -> [(Id, Id)]
sourceFields rt checked = [(poly, carrier) | ab <- collectAst (tm_typechecked_source checked),
    ABE poly _ _ _ <- abs_exports ab,
    FunBind (inner, _) name _ <- collectAst (abs_binds ab) :: [HsBind GhcTc], getLocA name `elem` locations,
    carrier <- wrapperVars inner]
  where
    wrapperVars (WpCompose a b) = wrapperVars a ++ wrapperVars b
    wrapperVars (WpEvLam v) = [v]
    wrapperVars _ = []
    locations = [getLocA name | Just (group, _, _, _, _) <- [tm_renamed_source checked],
        inst <- collectAst group :: [ClsInstDecl GhcRn], L _ (FunBind _ name _) <- toList (cid_binds inst),
        roleOf rt (unLoc name) == Just ReprMethod]

-- At a public call the actual type arguments must be closed. Inside a
-- certified field they can also depend on its instance parameters: the field
-- receives those from the same concrete DFun application. Local polymorphism
-- and other given dictionaries cannot supply such a substitution.
selectedHome :: HomeReprs -> Maybe Name -> Name -> [Type] -> Type -> Bool
selectedHome (HomeReprs heads scopes) caller owner arguments ty = case Map.lookup owner heads of
    Just (variables, headTy) -> length variables == length arguments && all inScope arguments &&
        eqType (substTyWith variables arguments headTy) ty
    Nothing -> False
  where
    scope = maybe emptyVarSet (\binder -> Map.findWithDefault emptyVarSet binder scopes) caller
    inScope t = tyCoVarsOfType t `subVarSet` scope

methodCarrier :: HomeReprs -> Name -> Bool
methodCarrier (HomeReprs _ scopes) = (`Map.member` scopes)
