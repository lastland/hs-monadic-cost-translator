-- | Exact compiler-built DataToTag dictionaries, separate from instance DFuns.
module ConstructionTag (tagDictionary, tagMethod, validTag) where

import Control.Monad (guard)
import GHC hiding (exprType)
import GHC.Builtin.Names (dataToTagClassName)
import GHC.Builtin.PrimOps (PrimOp(..))
import GHC.Builtin.PrimOps.Ids (primOpId)
import GHC.Builtin.Types (liftedDataConTy)
import GHC.Builtin.Types.Prim (intPrimTy)
import qualified GHC.Core as C
import GHC.Core.Class (classAllSelIds, classTyCon)
import GHC.Core.Coercion (coercionLKind, coercionRKind)
import GHC.Core.Coercion.Axiom (toBranchedAxiom)
import GHC.Core.DataCon (dataConWorkId)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep (Coercion(..), MCoercion(..))
import GHC.Core.TyCon (newTyConCo_maybe, tyConSingleDataCon, tyConFamilySize)
import GHC.Core.Type (mkVisFunTyMany, splitTyConApp_maybe)
import GHC.Core.Utils (exprType)
import GHC.Platform (Platform)
import GHC.StgToCmm.Closure (isSmallFamily)

tagMethod :: Name -> Type -> Bool
tagMethod method predicate = case getClassPredTys_maybe predicate of
    Just (cls, [levity, _]) -> getName cls == dataToTagClassName &&
        eqType levity liftedDataConTy && map getName (classAllSelIds cls) == [method]
    _ -> False

tagDictionary :: C.CoreExpr -> Maybe (Type, Type, Id)
tagDictionary expression = do
    let predicate = exprType expression
    (cls, [levity, operand]) <- getClassPredTys_maybe predicate
    guard (getName cls == dataToTagClassName && eqType levity liftedDataConTy &&
        length (classAllSelIds cls) == 1)
    (tc, []) <- splitTyConApp_maybe operand
    guard (null (tyConTyVars tc))
    payload <- case expression of
        C.Cast body co@(SymCo (AxiomInstCo actual 0 args)) -> do
            ax <- newTyConCo_maybe (classTyCon cls)
            types <- mapM reflexive args
            guard (actual == toBranchedAxiom ax && sameTypes types [levity, operand])
            guard (eqType (coercionLKind co) (exprType body) && eqType (coercionRKind co) predicate)
            pure body
        _ -> do
            (C.Var worker, [C.Type l, C.Type t, C.Cast body co]) <- pure (C.collectArgs expression)
            guard (worker == dataConWorkId (tyConSingleDataCon (classTyCon cls)) &&
                sameTypes [l, t] [levity, operand])
            _ <- reflexive co
            guard (eqType (coercionLKind co) (exprType body) && eqType (coercionRKind co) (exprType body))
            pure body
    (C.Var op, [C.Type l, C.Type t]) <- pure (C.collectArgs payload)
    guard (op `elem` map primOpId [DataToTagSmallOp, DataToTagLargeOp] &&
        sameTypes [l, t] [levity, operand] &&
        eqType (exprType payload) (mkVisFunTyMany operand intPrimTy))
    pure (predicate, operand, op)
  where
    reflexive (Refl ty) = Just ty
    reflexive (GRefl _ ty MRefl) = Just ty
    reflexive _ = Nothing
    sameTypes xs ys = length xs == length ys && and (zipWith eqType xs ys)

validTag :: Platform -> [TyCon] -> Type -> Id -> Bool
validTag platform homes operand op = case splitTyConApp_maybe operand of
    Just (tc, []) -> tc `elem` homes &&
        op == primOpId (if isSmallFamily platform (tyConFamilySize tc) then DataToTagSmallOp else DataToTagLargeOp)
    _ -> False
