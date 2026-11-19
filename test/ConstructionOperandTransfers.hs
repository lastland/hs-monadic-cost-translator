-- | Replay the ordinary callee's operand obligations, not the caller's role.
-- In particular a property can construct and observe its own generator, but
-- calling an ordinary home function does not turn that function into a property.
module ConstructionOperandTransfers (ordinaryOperandTransfers) where

import ConstructionAst (children, collectAst, pretty, typedVariable)
import ConstructionRoles (QuickCheck, isQuickCheckBinding)
import ConstructionRuntime (describeName)
import Data.Data (Data, cast)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import GHC
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type (getTyVar_maybe, mkScaledFunTys, mkTyVarTy, splitAppTys, splitFunTys, splitPiTys, substTy)
import GHC.Core.Unify (tcMatchTy)
import qualified GHC.Data.Strict as Strict
import GHC.Hs.Syn.Type (hsLPatType, lhsExprType)
import GHC.Tc.Utils.TcType (tcSplitSigmaTy)
import GHC.Types.Id.Make (seqId)
import GHC.Types.SrcLoc (getBufSpan)
import GHC.Types.Var.Set (elemVarSet)

type Call = (SrcSpan, Name, Type)

-- The template contains the equation's skolems, shared with its operand
-- types. AbsBinds exports can use different quantified variables.
data Body = Body { template :: Type, operands :: [Type], calls :: [Call] }

ordinaryOperandTransfers :: (Type -> Bool) -> QuickCheck -> [TypecheckedModule] -> [String]
ordinaryOperandTransfers result qc modules = concatMap report sources
  where
    sources = map tm_typechecked_source modules
    bodies = Map.fromList [(getName (unLoc n), body) | source <- sources,
        FunBind _ n matches <- collectAst source :: [HsBind GhcTc], not (isQuickCheckBinding qc (unLoc n)),
        let MatchGroupTc args res _ = mg_ext matches
            scheme = mkScaledFunTys args res
            (observed, direct) = bodyUses qc matches
            body = Body scheme observed direct,
        firstOrder scheme]
    exports = Map.fromList [(getName poly, getName mono) | source <- sources, ABE poly mono _ _ <- collectAst source]
    resolve name = Map.findWithDefault name name exports
    instantiate (_, name, ty) = do
        let key = resolve name
        body <- Map.lookup key bodies
        let actual = rho ty
        if firstOrder actual then (key,body,) <$> tcMatchTy (template body) actual else Nothing

    -- For a first-order value only its outer type constructor decides the
    -- role. Carry open heads across statically named calls, including aliases.
    -- The domain is finite: only free variables of each retained template can
    -- be added. Recursive calls never require unfolding a growing type or RHS.
    open body ty = do
        v <- getTyVar_maybe (fst (splitAppTys ty))
        if v `elemVarSet` tyCoVarsOfType (template body) then Just v else Nothing
    seeds = Map.map (\body -> Set.fromList (mapMaybe (open body) (operands body))) bodies
    edges = Map.map (mapMaybe instantiate . calls) bodies
    close known =
        let step name body = Set.union (seeds Map.! name) $ Set.fromList
                [v | (child, _, sub) <- edges Map.! name, h <- Set.toList (known Map.! child),
                    Just v <- [open body (substTy sub (mkTyVarTy h))]]
            next = Map.mapWithKey step bodies
        in if next == known then known else close next
    heads = close seeds

    -- The occurrence is checked even inside a QC body. Report only an
    -- obligation that becomes QC at this instantiation. An already concrete
    -- operand is checked at its own ordinary declaration, not at every caller.
    report source = nub
        [pretty location ++ ": QuickCheck value used outside a generator or property: " ++ describeName name
            ++ " (operand in ordinary callee at this instantiation)"
        | e <- collectAst source, Just call@(location, name, _) <- [typedCall e],
          Just (key, body, sub) <- [instantiate call],
          let becomes ty = not (result ty) && result (substTy sub ty),
          any becomes (operands body) || any (becomes . mkTyVarTy) (heads Map.! key)]

rho :: Type -> Type
rho ty = let (_, _, body) = tcSplitSigmaTy ty in body

firstOrder :: Type -> Bool
firstOrder = all (null . fst . splitPiTys . scaledThing) . fst . splitFunTys

typedCall :: LHsExpr GhcTc -> Maybe Call
typedCall expr = do
    (location, name) <- typedVariable expr
    -- The direct-call rule concerns a written occurrence. A syntax expansion
    -- can also contain an unlocated compiler invocation (e.g. rebindable if).
    case getBufSpan location of
        Strict.Just _ -> pure (location, name, lhsExprType expr)
        Strict.Nothing -> Nothing

-- Retain every equation, its guards and its lexical intermediates. A binding
-- introduction is not an operand. In particular matching a pair does not use
-- the types of both fields; the RHS occurrence identifies the actual field.
bodyUses :: Data a => QuickCheck -> a -> ([Type], [Call])
bodyUses qc = walk
  where
    walk :: Data a => a -> ([Type], [Call])
    walk node
        | Just (FunBind _ n _) <- cast node :: Maybe (HsBind GhcTc), isQuickCheckBinding qc (unLoc n) = ([], [])
        | otherwise = own <> mconcat (children walk node)
      where
        own | Just e <- cast node, Just call@(_, name, ty) <- typedCall e =
                  (snd (splitPiTys ty) : [scaledThing first | name == getName seqId,
                      first:_ <- [fst (splitFunTys (rho ty))]], [call])
            | Just pat@(L _ BangPat{}) <- cast node :: Maybe (LPat GhcTc) = ([hsLPatType pat], [])
            | otherwise = ([], [])
