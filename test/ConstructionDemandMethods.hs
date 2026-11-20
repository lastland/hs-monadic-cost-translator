-- | Narrow stock-method proofs. A deriving request identifies a definition,
-- never a permission for a type or for a caller-supplied dictionary.
module ConstructionDemandMethods (runtimeDemandMethod, stockMethods, safeHomeType) where

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
import GHC.Core.Type (isLiftedTypeKind, splitTyConApp_maybe, splitForAllTyVars, splitFunTys, substTyWith)
import GHC.Core.TyCon (isDataTyCon, isFamInstTyCon, tyConResKind, tyConSingleDataCon)
import GHC.Types.Id (isDFunId)
import GHC.Tc.Types (tcg_insts)

-- This summary covers only the two pinned stock instances at Demand.hs:13-14.
-- The actual DFun head, substitution, selected method, and one field dictionary
-- must all match. The caller still proves the actual selected child method.
runtimeDemandMethod :: Runtime -> Name -> Id -> Type -> [Type] -> [Type] -> Maybe (Name, Type)
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
    guard (getName tc == runtimeName rt "Memorist.Data.Interface.Demand" "ThD")
    selected <- lookup method [(name, getName op) | (op, def) <- classOpItems cls,
        name <- getName op : [n | Just (n, _) <- [def]]]
    child <- case [(label, name) | label <- ["==", "showsPrec"], Just name <- [lookup ("Prelude." ++ label) (standardNames rt)],
            name `elem` map getName (classAllSelIds cls)] of
        [(_, name)] -> pure name
        _ -> Nothing
    guard (selected `elem` map getName (classAllSelIds cls))
    pure (child, field)

standardMethod :: Runtime -> Name -> Class -> Bool
standardMethod rt method cls = any (\label -> case lookup ("Prelude." ++ label) (standardNames rt) of
    Just selector -> any (\(op, def) -> getName op == selector &&
        method `elem` (selector : [name | Just (name, _) <- [def]])) (classOpItems cls)
    Nothing -> False) ["==", "/=", "show", "showsPrec", "showList"]

-- Closed ordinary data only. The sole recursive field form is ThD applied to
-- this exact home type; every sibling field is checked before replay starts.
safeHomeType :: Runtime -> TyCon -> Bool
safeHomeType rt tc = isDataTyCon tc && not (isNewTyCon tc) &&
    not (isFamInstTyCon tc) && isLiftedTypeKind (tyConResKind tc) &&
    null (tyConTyVars tc) && not (null (tyConDataCons tc)) &&
    all (\dc -> isVanillaDataCon dc && all (safeField . scaledThing) (dataConInstOrigArgTys dc [])) (tyConDataCons tc)
  where
    safeField ty = scalarType ty || case splitTyConApp_maybe ty of
        Just (demand, [field]) -> getName demand == runtimeName rt "Memorist.Data.Interface.Demand" "ThD" &&
            (scalarType field || case splitTyConApp_maybe field of
                Just (home, []) -> home == tc
                _ -> False)
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
    guard ((getName cls, getName tc) `elem` requests && safeHomeType rt tc)
    (C.Var worker, C.Type actual : fields) <- [C.collectArgs dictionary]
    guard (worker == dataConWorkId (tyConSingleDataCon (classTyCon cls)) && eqType actual ty &&
        length fields == length (classAllSelIds cls))
    selected <- maybe [] pure (mapM (fieldBody cls fields) (zip (classAllSelIds cls) fields))
    pure (df, concat selected)
  where
    fieldBody cls fields (selector, C.Var implementation) = do
        guard (standardMethod rt (getName selector) cls)
        rhs <- Map.lookup (getName implementation) env
        -- A default wrapper is not a proof of its own default. Replay the
        -- substantive selected method on which the pinned standard default rests.
        let dependency label = do
                name <- lookup ("Prelude." ++ label) (standardNames rt)
                implementation' <- lookup name [(getName s, e) | (s, e) <- zip (classAllSelIds cls) fields]
                case implementation' of
                    C.Var v -> Map.lookup (getName v) env
                    _ -> Nothing
            defaultBody = case lookup (getName selector) [(n, label) |
                    (occ, label) <- [("/=", "=="), ("show", "showsPrec"), ("showList", "showsPrec")],
                    Just n <- [lookup ("Prelude." ++ occ) (standardNames rt)]] of
                Just label -> dependency label
                Nothing -> Just rhs
        defRhs <- defaultBody
        pure ((getName selector, rhs) : [(name, defRhs) |
            (op, Just (name, _)) <- classOpItems cls, op == selector])
    fieldBody _ _ _ = Nothing
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
