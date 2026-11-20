-- | Scalar marks an operand universe, not dictionary compliance. At these six
-- selectors callers promise to demand both outermost operands when Bool is
-- demanded, respecting thunk accounting. Visible bodies remain independently
-- checked: even a strict tval-observing comparison is outside that contract.
module ConstructionScalarComparison (scalarComparisonFacts, callerComparison) where

import ConstructionArithmetic (wellFormed)
import ConstructionAst (checkedModule, collectAst)
import ConstructionDictionary (Evidence(..), Use(..), predicateOf)
import ConstructionRuntime (Runtime, Family(..), scalarType, standardNames)
import GHC
import GHC.Builtin.Names (eqClassName, ordClassName)
import GHC.Builtin.Types (liftedTypeKind)
import GHC.Core.Class (classTyVars, classAllSelIds, classATItems)
import GHC.Core.InstEnv (is_cls, is_tys, is_dfun)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (splitForAllTyVars, splitFunTys, mkTyVarTy)
import GHC.Tc.Types (tcg_tcs, tcg_insts)
import GHC.Tc.Types.Evidence (HsWrapper(..))
import GHC.Types.Name (getOccString, nameModule_maybe)
import GHC.Types.Var (tyVarKind)
import System.FilePath (normalise)

-- Pair actual binders in the same checked abstraction, not printed predicates.
-- The marker must belong to the physical root, with exactly the emitted shape
-- and the seven distinct primitive instances across the complete home graph.
scalarComparisonFacts :: FilePath -> [TypecheckedModule] -> [(Name, Type)]
scalarComparisonFacts output modules =
    [(getName method, operand) | marker <- markers, m <- modules,
        binders <- groups m,
        scalar <- binders,
        Just (cls, [kind, operand]) <- [getClassPredTys_maybe (idType scalar)],
        cls == marker, eqType kind liftedTypeKind,
        method <- binders,
        Just (owner, [actual]) <- [getClassPredTys_maybe (idType method)],
        getName owner `elem` [eqClassName, ordClassName], eqType actual operand]
  where
    groups m = [abs_ev_vars ab | ab <- collectAst (tm_typechecked_source m)] ++
        [wrapperBinders w | w <- collectAst (tm_typechecked_source m)]
    wrapperBinders (WpEvLam v) = [v]
    wrapperBinders (WpCompose a b) = wrapperBinders a ++ wrapperBinders b
    wrapperBinders _ = []
    markers = [cls | m <- modules,
        let summary = pm_mod_summary (tm_parsed_module m),
        maybe False ((== normalise output) . normalise) (ml_hs_file (ms_location summary)),
        tc <- tcg_tcs (fst (tm_internals_ m)), getOccString tc == "Scalar",
        nameModule_maybe (getName tc) == Just (checkedModule m),
        Just cls <- [tyConClass_maybe tc], [k, v] <- [classTyVars cls],
        eqType (tyVarKind k) liftedTypeKind, eqType (tyVarKind v) (mkTyVarTy k),
        null (classSCTheta cls), null (classAllSelIds cls),
        null (classATItems cls), null (snd (classTvsFds cls)),
        let instances = [i | home <- modules, i <- tcg_insts (fst (tm_internals_ home)), is_cls i == cls],
        length instances == 7, all primitive instances,
        length [() | i <- instances, j <- instances, sameHead i j] == 7]
    sameHead i j = case (is_tys i, is_tys j) of
        ([ka, a], [kb, b]) -> eqType ka kb && eqType a b
        _ -> False
    primitive i = case is_tys i of
        [kind, ty] -> eqType kind liftedTypeKind && scalarType ty && case splitForAllTyVars (idType (is_dfun i)) of
            ([], body) -> null (fst (splitFunTys body))
            _ -> False
        _ -> False

callerComparison :: Runtime -> [(Name, Type)] -> Use -> Bool
callerComparison rt facts (Use selected cls ScalarMethod [ty] [evidence]) =
    any matches operations && wellFormed evidence && matching && operand evidence
  where
    operations = [(op, eqClassName) | op <- ["==", "/="]] ++
        [(op, ordClassName) | op <- ["<", "<=", ">", ">="]]
    matches (op, owner) = cls == owner &&
        lookup ("Prelude." ++ op) (standardNames rt) == Just selected
    matching = case predicateOf evidence >>= getClassPredTys_maybe of
        Just (actual, [argument]) -> getName actual == cls && eqType argument ty
        _ -> False
    operand _ | scalarType ty = True
    operand (Given binder _) = any (\(name, argument) -> name == binder && eqType argument ty) facts
    operand (Super _ _ parent) = operand parent
    operand _ = False
callerComparison _ _ _ = False
