-- | Narrow stock-method proofs. A deriving request identifies a definition,
-- never a permission for a type or for a caller-supplied dictionary.
module ConstructionDemandMethods (runtimeDemandMethod, StockMethods(..), Stock(..), StockMethod(..), stockMethods, safeHomeType, standardMethod) where

import ConstructionDictionary (Environment, Evidence(..), Use(..), sameEvidence)
import ConstructionRuntime
import ConstructionStockRequests (stockHead)
import Control.Monad (guard)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC hiding (exprType, typeKind)
import qualified GHC.Core as C
import GHC.Core.Class (classAllSelIds, classOpItems, classTyCon)
import GHC.Core.DataCon (dataConInstOrigArgTys, dataConWorkId)
import GHC.Core.InstEnv (is_dfun)
import GHC.Core.Predicate (getClassPredTys_maybe, mkClassPred)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type (isPredTy, mkTyVarTy, typeKind, isLiftedTypeKind, splitTyConApp_maybe, splitForAllTyVars, splitFunTys, substTyWith)
import GHC.Core.TyCon (isDataTyCon, isFamInstTyCon, tyConResKind, tyConSingleDataCon)
import GHC.Types.Id (isDFunId)
import GHC.Types.Var (isId, isTyVar, varType)
import GHC.Core.Utils (exprType)
import GHC.Tc.Types (tcg_insts)

-- Pinned Demand.hs:13-14,59-60,81 and List.hs:30-31 derive Eq/Show.
-- ThD/MaybeD delegate to their payload; PairD delegates to BOTH ordered
-- payloads. ListD's constructor-guarded tail recurs at exactly ListD a,
-- through ThD, with the same dictionary. Its audited summary exposes a
-- single payload obligation, not a general home-recursion permission.
-- The exact bound DFun head, lifted forall parameters, ordered context and
-- substitution must match. Every returned child is still proved by replay.
runtimeDemandMethod :: Runtime -> Name -> Id -> Type -> [Type] -> [Type] -> Maybe [(Name, Type)]
runtimeDemandMethod rt method df predicate args children = do
    (cls, [ty]) <- getClassPredTys_maybe predicate
    guard (standardMethod rt method cls)
    (boundClass, [template]) <- boundInstance rt (getName df)
    guard (boundClass == getName cls && runtimeOwns rt (getName df))
    let (vars, body) = splitForAllTyVars (idType df)
        (context, result) = splitFunTys body
    guard (length vars == length args && all (isLiftedTypeKind . varType) vars &&
        all (isLiftedTypeKind . typeKind) args && not (isProtected rt ty))
    let apply = substTyWith vars args
    guard (eqType (apply result) predicate &&
        eqType (apply template) ty &&
        and (zipWith eqType (map (apply . scaledThing) context) children))
    (tc, fields) <- splitTyConApp_maybe ty
    arity <- lookup (getName tc)
        ([(runtimeName rt "Memorist.Data.Interface.Demand" label, n) |
            (label, n) <- [("ThD", 1), ("MaybeD", 1), ("PairD", 2)]] ++
         [(runtimeName rt "Memorist.Data.List" "ListD", 1)])
    (templateTc, templateFields) <- splitTyConApp_maybe template
    guard (templateTc == tc && length fields == arity &&
        length vars == arity && length templateFields == arity &&
        and (zipWith eqType templateFields (map mkTyVarTy vars)) &&
        length context == arity && length children == arity &&
        and (zipWith eqType (map scaledThing context)
            (map (mkClassPred cls . pure . mkTyVarTy) vars)) &&
        and (zipWith eqType children (map (mkClassPred cls . pure) fields)))
    selected <- lookup method [(name, getName op) | (op, def) <- classOpItems cls,
        name <- getName op : [n | Just (n, _) <- [def]]]
    child <- case [(label, name) | label <- ["==", "showsPrec"], Just name <- [lookup ("Prelude." ++ label) (standardNames rt)],
            name `elem` map getName (classAllSelIds cls)] of
        [(_, name)] -> pure name
        _ -> Nothing
    guard (selected `elem` map getName (classAllSelIds cls))
    pure [(child, field) | field <- fields]

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

stockMethods :: Runtime -> Set.Set Name -> TypecheckedModule -> Environment -> (C.CoreExpr -> [Use]) -> StockMethods
stockMethods rt provenance checked env collect = StockMethods env stocks
  where
    stocks = do
        inst <- tcg_insts (fst (tm_internals_ checked))
        let df = is_dfun inst
        guard (isDFunId df && Set.member (getName df) provenance)
        dictionary <- maybe [] pure (Map.lookup (getName df) env)
        guard (eqType (exprType dictionary) (idType df))
        let (bs, body) = C.collectBinders dictionary
            (vars, slots) = span isTyVar bs
        guard (all (\v -> isId v && isPredTy (idType v)) slots)
        (cls, _, _, _) <- maybe [] pure (stockHead vars (exprType body))
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
            sameTypes (map (ty . varType) vars) (map varType methodVars) &&
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
