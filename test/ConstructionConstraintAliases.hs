-- | Evidence aliases from an exact source-defined constraint eliminator.
-- Inspect the full body and callsite, never its spelling or an unfolding.
module ConstructionConstraintAliases (constraintAliases) where

import Control.Monad (guard)
import ConstructionDictionary (trace, hasApplication)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC hiding (exprType, typeKind)
import qualified GHC.Core as C
import GHC.Core.FVs (exprFreeVars)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (isPredTy, substTyWith, typeKind)
import GHC.Core.Utils (exprType, cheapEqExpr)
import GHC.Types.Var (isId, isTyVar, varType)
import GHC.Types.Unique.Set (nonDetEltsUniqSet)

constraintAliases :: Map.Map Name C.CoreExpr -> Map.Map Name C.CoreExpr
constraintAliases env = Map.mapMaybe unique (Map.fromListWith (++)
    [(getName binder, [source]) | rhs <- Map.elems env, expression <- expressions rhs,
        Just (binder, source) <- [alias expression]])
  where
    roots = Set.fromList [getName v | rhs <- Map.elems env,
        v <- nonDetEltsUniqSet (exprFreeVars rhs), isId v, isPredTy (idType v)]
    unique (rhs:rest) | all (cheapEqExpr rhs) rest = Just rhs
    unique _ = Nothing
    alias expression = do
        (C.Var helper, [C.Type predicate, C.Type result, source, token, continuation]) <-
            pure (C.collectArgs expression)
        definition <- Map.lookup (getName helper) env
        guard (eqType (idType helper) (exprType definition))
        ([c, r, dictionary, ignored, callback], local, body) <- telescope [] Map.empty definition
        guard (all isTyVar [c, r] && all isId [dictionary, ignored, callback])
        C.App (C.Var selectedCallback) (C.Var selectedDictionary) <- pure body
        guard (selectedCallback == callback &&
            Map.findWithDefault selectedDictionary (getName selectedDictionary) local == dictionary)
        let instantiate = substTyWith [c, r] [predicate, result]
        guard (eqType (varType c) (typeKind predicate) &&
            eqType (instantiate (varType r)) (typeKind result) && isPredTy predicate)
        guard (and (zipWith (\v actual -> eqType (instantiate (idType v)) (exprType actual))
            [dictionary, ignored, callback] [source, token, continuation]))
        guard (eqType predicate (exprType source) &&
            eqType result (instantiate (exprType body)) && eqType result (exprType expression))
        C.Lam binder _ <- pure continuation
        guard (isId binder && isPredTy (idType binder) && eqType predicate (idType binder))
        -- This slice recovers only an applied quantified proof, not arbitrary
        -- concrete dictionaries passed to otherwise identical callbacks.
        guard (hasApplication (trace env roots Set.empty source))
        pure (binder, source)
    telescope binders local (C.Lam v body) = telescope (binders ++ [v]) local body
    telescope binders local (C.Let (C.NonRec v (C.Var source)) body) = do
        guard (isId v && isPredTy (idType v) && eqType (idType v) (idType source) &&
            (source `elem` binders || Map.member (getName source) local))
        telescope binders (Map.insert (getName v)
            (Map.findWithDefault source (getName source) local) local) body
    telescope binders local body = Just (binders, local, body)
    expressions e = e : case e of
        C.App f x -> expressions f ++ expressions x
        C.Lam _ body -> expressions body
        C.Let b body -> concatMap expressions (C.rhssOfBind b) ++ expressions body
        C.Case scrutinee _ _ alts -> expressions scrutinee ++ concatMap expressions (C.rhssOfAlts alts)
        C.Cast body _ -> expressions body
        C.Tick _ body -> expressions body
        _ -> []
