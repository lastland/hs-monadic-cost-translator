-- | Narrow stock-method proofs. A deriving request identifies a definition,
-- never a permission for a type or for a caller-supplied dictionary.
module ConstructionDemandMethods (runtimeDemandMethod, stockMethods) where

import ConstructionAst (collectAst)
import ConstructionRuntime
import Control.Monad (guard)
import qualified Data.Map.Strict as Map
import GHC
import qualified GHC.Core as C
import GHC.Core.Class (classAllSelIds, classOpItems, classTyCon)
import GHC.Core.DataCon (dataConInstOrigArgTys, dataConWorkId)
import GHC.Core.InstEnv (is_dfun)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type (splitTyConApp_maybe, splitForAllTyVars, splitFunTys, substTyWith)
import GHC.Core.TyCon (isDataTyCon, tyConSingleDataCon)
import GHC.Types.Id (isDFunId)
import GHC.Tc.Types (tcg_insts)

-- This summary covers only the two pinned stock instances at Demand.hs:13-14.
-- The actual DFun head, substitution, selected method, and one field dictionary
-- must all match. The caller still proves that dictionary recursively.
runtimeDemandMethod :: Runtime -> Name -> Id -> Type -> [Type] -> [Type] -> Maybe Type
runtimeDemandMethod rt method df predicate args children = do
    (cls, [ty]) <- getClassPredTys_maybe predicate
    guard (standardMethod rt method cls)
    (boundClass, [template]) <- boundInstance rt (getName df)
    guard (boundClass == getName cls && runtimeOwns rt (getName df))
    let (vars, body) = splitForAllTyVars (idType df)
        (context, result) = splitFunTys body
    guard (length vars == length args && length context == 1 && length children == 1)
    let apply = substTyWith vars args
    guard (eqType (apply result) predicate &&
        eqType (apply template) ty &&
        and (zipWith eqType (map (apply . scaledThing) context) children))
    (tc, [field]) <- splitTyConApp_maybe ty
    guard (getName tc == runtimeName rt "Memorist.Data.Interface.Demand" "ThD" && scalarType field)
    pure field

standardMethod :: Runtime -> Name -> Class -> Bool
standardMethod rt method cls = any (\label -> case lookup ("Prelude." ++ label) (standardNames rt) of
    Just selector -> any (\(op, def) -> getName op == selector &&
        method `elem` (selector : [name | Just (name, _) <- [def]])) (classOpItems cls)
    Nothing -> False) ["==", "/=", "show", "showsPrec", "showList"]

-- Closed data only: no newtype/coercion, type variables, recursion, callbacks or
-- unexamined home fields. This intentionally is not a general deriving engine.
safeField :: Runtime -> Type -> Bool
safeField rt ty = scalarType ty || demandField rt ty

demandField :: Runtime -> Type -> Bool
demandField rt ty = case splitTyConApp_maybe ty of
    Just (tc, [field]) -> getName tc == runtimeName rt "Memorist.Data.Interface.Demand" "ThD" && scalarType field
    _ -> False

stockMethods :: Runtime -> TypecheckedModule -> Map.Map Name C.CoreExpr -> [(Id, [(Name, C.CoreExpr)])]
stockMethods rt checked env = do
    (dfName, dictionary) <- Map.toList env
    -- Recover the actual Id from the dictionary's constructor type and the
    -- module's checked instance environment, not a DFun spelling.
    df <- [v | instance' <- tcg_insts (fst (tm_internals_ checked)), let v = is_dfun instance', getName v == dfName, isDFunId v]
    ([], body) <- [splitForAllTyVars (idType df)]
    ([], predicate) <- [splitFunTys body]
    (cls, [ty]) <- maybe [] pure (getClassPredTys_maybe predicate)
    (tc, []) <- maybe [] pure (splitTyConApp_maybe ty)
    let fieldsOf dc = map scaledThing (dataConInstOrigArgTys dc [])
    guard ((getName cls, getName tc) `elem` requests && isDataTyCon tc && not (isNewTyCon tc) &&
        null (tyConTyVars tc) && not (null (tyConDataCons tc)) &&
        all (\dc -> isVanillaDataCon dc && all (safeField rt) (fieldsOf dc)) (tyConDataCons tc) &&
        any (any (demandField rt) . fieldsOf) (tyConDataCons tc))
    (C.Var worker, C.Type actual : fields) <- [C.collectArgs dictionary]
    guard (worker == dataConWorkId (tyConSingleDataCon (classTyCon cls)) && eqType actual ty &&
        length fields == length (classAllSelIds cls))
    selected <- maybe [] pure (mapM (fieldBody cls) (zip (classAllSelIds cls) fields))
    pure (df, concat selected)
  where
    fieldBody cls (selector, C.Var implementation) = do
        guard (standardMethod rt (getName selector) cls)
        rhs <- Map.lookup (getName implementation) env
        pure [(n, rhs) | (op, def) <- classOpItems cls, op == selector,
            n <- getName selector : [name | Just (name, _) <- [def]]]
    fieldBody _ _ = Nothing
    -- On a data declaration the actual standard Eq/Show classes select stock
    -- derivation by default. Explicit anyclass/via/newtype strategies do not
    -- establish that proof. A manual instance cannot coexist with this request:
    -- GHC rejects the duplicate before this checker can issue a certificate.
    requests = case tm_renamed_source checked of
        Nothing -> []
        Just (group, _, _, _, _) ->
            [(cls, unLoc target) |
                DataDecl{tcdLName = target, tcdDataDefn = definition} <- collectAst group :: [TyClDecl GhcRn],
                L _ clause <- dd_derivs definition,
                stock (deriv_clause_strategy clause),
                L _ HsSig{sig_body = L _ (HsTyVar _ _ name)} <- collectAst (deriv_clause_tys clause) :: [LHsSigType GhcRn],
                let cls = unLoc name] ++
            [(unLoc cls, unLoc target) |
                declaration <- collectAst group :: [DerivDecl GhcRn], stock (deriv_strategy declaration),
                L _ HsSig{sig_body = L _ (HsAppTy _ (L _ (HsTyVar _ _ cls)) (L _ (HsTyVar _ _ target)))} <-
                    collectAst (deriv_type declaration) :: [LHsSigType GhcRn]]
    stock :: Maybe (LDerivStrategy GhcRn) -> Bool
    stock Nothing = True
    stock (Just (L _ StockStrategy{})) = True
    stock _ = False
