-- | Narrow stock-method proofs. A deriving request identifies a definition,
-- never a permission for a type or for a caller-supplied dictionary.
module ConstructionDemandMethods (runtimeDemandMethod, StockMethods(..), Stock(..), StockMethod(..), stockMethods, safeHomeType, standardMethod) where

import ConstructionAst (collectAst)
import ConstructionDictionary (Environment, Evidence(..), Use(..), sameEvidence)
import ConstructionRuntime
import Control.Monad (guard)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import GHC hiding (exprType)
import qualified GHC.Core as C
import GHC.Core.Class (classAllSelIds, classOpItems, classTyCon)
import GHC.Core.DataCon (dataConInstOrigArgTys, dataConWorkId)
import GHC.Core.InstEnv (is_dfun)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type (isPredTy, mkTyVarTy, isLiftedTypeKind, splitTyConApp_maybe, splitForAllTyVars, splitFunTys, substTyWith)
import GHC.Core.TyCon (isDataTyCon, isFamInstTyCon, tyConResKind, tyConSingleDataCon)
import GHC.Types.Id (isDFunId)
import GHC.Types.Var (isId, isTyVar, varType)
import GHC.Core.Utils (exprType)
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

-- One catalogue serves declaration checking and concrete selected-field replay.
-- Implementation identities and their own formal slots come from actual fields,
-- never generated method spellings or source locations.
data StockMethod = StockMethod Name (Maybe Name) Id [TyVar] [Id] [Use] (Maybe Name)
data Stock = Stock Id [TyVar] [Id] Type [StockMethod]
data StockMethods = StockMethods Environment [Stock]

stockMethods :: Runtime -> TypecheckedModule -> Environment -> (C.CoreExpr -> [Use]) -> StockMethods
stockMethods rt checked env collect = StockMethods env stocks
  where
    stocks = do
        inst <- tcg_insts (fst (tm_internals_ checked))
        let df = is_dfun inst
        guard (isDFunId df)
        dictionary <- maybe [] pure (Map.lookup (getName df) env)
        let (bs, body) = C.collectBinders dictionary
            (vars, slots) = span isTyVar bs
        guard (all (\v -> isId v && isPredTy (idType v)) slots)
        (cls, [operand]) <- maybe [] pure (getClassPredTys_maybe (exprType body))
        (tc, args) <- maybe [] pure (splitTyConApp_maybe operand)
        guard (isDataTyCon tc && not (isNewTyCon tc) && not (isFamInstTyCon tc) &&
            isLiftedTypeKind (tyConResKind tc) && all (isLiftedTypeKind . varType) vars &&
            length args == length (tyConTyVars tc) && sameTypes args (map mkTyVarTy vars) &&
            all isVanillaDataCon (tyConDataCons tc) &&
            (getName cls, getName tc, length args) `elem` requests)
        (C.Var worker, supplied) <- [C.collectArgs body]
        guard (worker == dataConWorkId (tyConSingleDataCon (classTyCon cls)))
        let fields = filter (not . C.isTyCoArg) supplied
        guard (length fields == length (classAllSelIds cls))
        methods <- maybe [] pure (mapM (field df vars slots (exprType body) cls) (zip (classAllSelIds cls) fields))
        pure (Stock df vars slots (exprType body) methods)
    field df vars slots predicate cls (selector, expression) = do
        guard (standardMethod rt (getName selector) cls)
        (C.Var implementation, supplied) <- pure (C.collectArgs expression)
        let typeArgs = [t | C.Type t <- supplied]
            evArgs = filter (not . C.isTyCoArg) supplied
        guard (length supplied == length vars + length slots &&
            sameTypes typeArgs (map mkTyVarTy vars) &&
            length evArgs == length slots && and (zipWith sameVar evArgs slots))
        rhs <- Map.lookup (getName implementation) env
        let (binders, _) = C.collectBinders rhs
            (methodVars, afterTypes) = span isTyVar binders
            methodSlots = take (length slots) afterTypes
            ty = substTyWith vars (map mkTyVarTy methodVars)
        guard (length methodVars == length vars && length methodSlots == length slots &&
            all isId methodSlots && sameTypes (map idType methodSlots) (map (ty . idType) slots))
        let obligations = collect rhs
            self = Instance df (ty predicate) (map mkTyVarTy methodVars)
                [Given (getName v) (idType v) | v <- methodSlots]
            def = lookup selector [(s, n) | (s, Just (n, _)) <- classOpItems cls]
            inherited = case obligations of
                [Use selected actual ScalarMethod [t] [d]] |
                    def == Just selected, actual == getName cls,
                    Just (_, [target]) <- getClassPredTys_maybe (ty predicate),
                    eqType t target, sameEvidence d self -> Just selected
                _ -> Nothing
        pure (StockMethod (getName selector) def implementation methodVars methodSlots obligations inherited)
    sameTypes xs ys = length xs == length ys && and (zipWith eqType xs ys)
    sameVar (C.Var actual) expected = actual == expected
    sameVar _ _ = False
    -- Only stock requests on fully generic ordinary data heads qualify.
    requests = case tm_renamed_source checked of
        Nothing -> []
        Just (group, _, _, _, _) ->
            [(cls, unLoc target, length (hsQTvExplicit variables)) |
                DataDecl{tcdLName = target, tcdTyVars = variables, tcdDataDefn = definition} <-
                    collectAst group :: [TyClDecl GhcRn],
                L _ clause <- dd_derivs definition, stock (deriv_clause_strategy clause),
                L _ HsSig{sig_body = sig} <- collectAst (deriv_clause_tys clause) :: [LHsSigType GhcRn],
                Just (cls, []) <- [typeApps sig]] ++
            [(cls, target, length vars) |
                declaration <- collectAst group :: [DerivDecl GhcRn], stock (deriv_strategy declaration),
                L _ HsSig{sig_body = sig} <- collectAst (deriv_type declaration) :: [LHsSigType GhcRn],
                Just (cls, [operand]) <- [typeApps (unqualified sig)],
                Just (target, arguments) <- [typeApps operand],
                Just vars <- [mapM typeVariable arguments], length (nub vars) == length vars]
    stock :: Maybe (LDerivStrategy GhcRn) -> Bool
    stock Nothing = True
    stock (Just (L _ StockStrategy{})) = True
    stock _ = False
    unqualified (L _ (HsForAllTy _ _ body)) = unqualified body
    unqualified (L _ (HsQualTy _ _ body)) = unqualified body
    unqualified body = body
    typeApps (L _ (HsTyVar _ _ n)) = Just (unLoc n, [])
    typeApps (L _ (HsAppTy _ f x)) = do (n, xs) <- typeApps f; pure (n, xs ++ [x])
    typeApps (L _ (HsParTy _ t)) = typeApps t
    typeApps _ = Nothing
    typeVariable (L _ (HsTyVar _ _ n)) = Just (unLoc n)
    typeVariable (L _ (HsParTy _ t)) = typeVariable t
    typeVariable _ = Nothing
