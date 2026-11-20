-- | Finite closed instantiations of private inferred action groups. This proof
-- substitutes existing obligations; it never certifies a dictionary's methods.
module ConstructionLocalInstances
    ( LocalGroup, LocalInstances, localGroups, requestedLocals, localInstances, instantiateLocals ) where

import ConstructionArithmetic (wellFormed)
import ConstructionAst (collectAst, firstOrderCall, variable, typedVariable)
import ConstructionDictionary (Environment, Evidence, Use(..), trace, predicateOf, substitute, sameEvidence)
import ConstructionRuntime (Runtime, scalarType, isMemoristType)
import Control.Monad (guard, zipWithM)
import Data.Foldable (toList)
import Data.List (nubBy)
import qualified Data.Set as Set
import GHC hiding (exprType)
import qualified GHC.Core as C
import GHC.Core.Coercion (topNormaliseNewType_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.Type (isPredTy, isPiTy, splitFunTys, splitAppTy_maybe, splitTyConApp_maybe, substTyWith)
import GHC.Core.TyCo.Rep (scaledThing)
import qualified GHC.Core.TyCo.Subst as T
import GHC.Core.Utils (exprType)
import qualified GHC.Data.Strict as Strict
import GHC.Hs.Syn.Type (lhsExprType)
import GHC.Types.Name (isInternalName)
import GHC.Types.SrcLoc (getBufSpan, BufSpan)
import GHC.Types.Var.Set (elemVarSet)

data LocalGroup = LocalGroup [TyVar] [Id] [(Id, Id, Int)] [(Name, LHsExpr GhcTc)] Bool
data LocalInstances = LocalInstances [TyVar] [Id] [([Type], [Evidence])]

-- Actual monomorphic siblings share their group's skolems/dictionary binders.
-- Local inferred recursion therefore does not need a recursive evaluator.
localGroups :: Runtime -> TypecheckedModule -> [LocalGroup]
localGroups rt checked =
    [LocalGroup (abs_tvs ab) (abs_ev_vars ab) exports occurrences (all goodOccurrence occurrences) |
        ab <- collectAst (tm_typechecked_source checked) :: [AbsBinds],
        not (null (abs_tvs ab)), not (null (abs_ev_vars ab)),
        let bodies = [(unLoc name, matches) | L _ (FunBind _ name matches) <- toList (abs_binds ab)],
        let exports = [(poly, mono, matchGroupArity matches) | ABE poly mono _ _ <- abs_exports ab,
                (actual, matches) <- bodies, getName actual == getName mono],
        length exports == length (abs_exports ab), not (null exports),
        all (\(poly, mono, arity) -> isInternalName (getName poly) && arity > 0 &&
            finiteAction arity (idType mono)) exports,
        let names = [getName v | (poly, mono, _) <- exports, v <- [poly, mono]],
        let occurrences = [(name, e) | e <- collectAst (tm_typechecked_source checked),
                Just (_, name) <- [typedVariable e], name `elem` names],
        let goodOccurrence (name, e) = case typedVariable e of
                Just (at, _) | Strict.Just physical <- getBufSpan at ->
                    any (\(poly, mono, arity) -> name `elem` map getName [poly, mono] &&
                        (name == getName poly || eqType (lhsExprType e) (idType mono)) &&
                        (physical, getName poly, arity) `elem` saturated) exports
                _ -> False]
  where
    finiteAction arity ty =
        let (args, result) = splitFunTys ty
        in length args == arity && all (firstOrderValue . scaledThing) args &&
            case splitAppTy_maybe result of
                Just (carrier, payload) -> isMemoristType rt carrier && firstOrderValue payload
                _ -> False
    firstOrderValue ty = not (isPiTy ty) &&
        not (maybe False (isPiTy . snd) (topNormaliseNewType_maybe ty)) &&
        case splitTyConApp_maybe ty of
            Just (tc, _) -> not (isTypeFamilyTyCon tc)
            _ -> True
    -- Names plus physical buffer positions cannot be forged by LINE pragmas.
    saturated :: [(BufSpan, Name, Int)]
    saturated = [(physical, name, length args) |
        Just (group, _, _, _, _) <- [tm_renamed_source checked],
        e <- collectAst group :: [LHsExpr GhcRn],
        let (head', args) = firstOrderCall e, not (null args),
        Just name <- [variable head'], Strict.Just physical <- [getBufSpan (getLocA head')]]

requestedLocals :: [LocalGroup] -> [LHsExpr GhcTc]
requestedLocals groups = [e | LocalGroup _ _ exports occurrences _ <- groups,
    (name, e) <- occurrences, any (\(poly, _, _) -> getName poly == name) exports]

localInstances :: Environment -> Set.Set Name -> [LocalGroup] -> [C.CoreExpr] -> [LocalInstances]
localInstances env given groups expressions = concat (fst (foldl collect ([], expressions) groups))
  where
    collect (facts, remaining) (LocalGroup vars slots exports occurrences saturated) =
        let external = [e | (name, e) <- occurrences, any (\(poly, _, _) -> getName poly == name) exports]
            (actual, rest) = splitAt (length external) remaining
            contexts = zipWithM (context vars slots exports) external actual
            fact = case contexts of
                Just cs | saturated, not (null cs), length actual == length external ->
                    [LocalInstances vars slots (nubBy sameContext cs)]
                _ -> []
        in (facts ++ [fact], rest)
    context vars slots exports source expression = do
        let (head', args) = C.collectArgs expression
        C.Var selected <- pure head'
        (_, mono, _) <- unique [(poly, mono, arity) | (poly, mono, arity) <- exports, getName poly == getName selected]
        let types = [ty | C.Type ty <- args]
            dictionaries = [a | a <- args, not (C.isTyCoArg a)]
        guard (length types == length vars && all scalarType types &&
            length dictionaries == length slots && all (isPredTy . exprType) dictionaries)
        let ty = substTyWith vars types
            evidence = map (trace env given Set.empty) dictionaries
        guard (and (zipWith (\slot ev -> wellFormed ev &&
            maybe False (eqType (ty (idType slot))) (predicateOf ev)) slots evidence))
        guard (eqType (exprType expression) (ty (idType mono)) &&
            eqType (lhsExprType source) (exprType expression))
        pure (types, evidence)
    sameContext (as, ds) (bs, es) = length as == length bs && and (zipWith eqType as bs) &&
        length ds == length es && and (zipWith sameEvidence ds es)
    unique [x] = Just x
    unique _ = Nothing

-- Every sibling obligation is checked in every incoming closed context.
-- With no complete certificate the original (usually open) obligation survives.
instantiateLocals :: [LocalInstances] -> Use -> [Use]
instantiateLocals facts use = foldl step [use] facts
  where
    step uses' (LocalInstances vars slots contexts) = concatMap instantiate uses'
      where
        instantiate original@(Use name cls family types dictionaries)
            | any mentions (types ++ [p | ev <- dictionaries, Just p <- [predicateOf ev]]) =
                [Use name cls family (map ty types)
                    (map (substitute (T.zipTvSubst vars arguments) (zip (map getName slots) evidence)) dictionaries) |
                    (arguments, evidence) <- contexts, let ty = substTyWith vars arguments]
            | otherwise = [original]
        mentions ty = any (`elemVarSet` tyCoVarsOfType ty) vars
