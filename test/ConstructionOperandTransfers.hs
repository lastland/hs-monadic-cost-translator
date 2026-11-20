-- | Replay the ordinary callee's operand obligations, not the caller's role.
-- In particular a property can construct and observe its own generator, but
-- calling an ordinary home function does not turn that function into a property.
module ConstructionOperandTransfers (ordinaryOperandTransfers) where

import ConstructionAst (children, collectAst, pretty, strictConstructorOperands, strictNullaryRhs, typedVariable, updateAlternatives)
import ConstructionQuickCheckUses (quickCheckOperand)
import ConstructionRoles (QuickCheck, isQuickCheckBinding)
import ConstructionRuntime (describeName)
import Data.Data (Data, cast)
import Data.Foldable (toList)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import GHC
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.DataCon (dataConImplBangs, dataConOrigArgTys, dataConOrigResTy, isBanged)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type (getTyVar_maybe, mkScaledFunTys, mkTyVarTy, splitAppTys, splitFunTys, splitPiTys, substTy)
import GHC.Core.Unify (tcMatchTy)
import qualified GHC.Data.Strict as Strict
import GHC.Hs.Syn.Type (hsLPatType, lhsExprType)
import GHC.Tc.Utils.TcType (tcSplitSigmaTy)
import GHC.Types.FieldLabel (flSelector)
import GHC.Types.Id.Make (seqId)
import GHC.Types.Name.Reader (greName)
import GHC.Types.SrcLoc (getBufSpan)
import GHC.Types.Var.Set (elemVarSet)

type Call = (SrcSpan, Name, Type)

-- The template contains the equation's skolems, shared with its operand
-- types. AbsBinds exports can use different quantified variables.
data Body = Body { template :: Type, forcedOperands :: [Type], calls :: [Call] }

ordinaryOperandTransfers :: (Type -> Bool) -> QuickCheck -> [TypecheckedModule] -> [String]
ordinaryOperandTransfers result qc modules = concatMap report sources
  where
    sources = map tm_typechecked_source modules
    bodies = Map.fromList (concatMap (mapMaybe (ordinaryBody qc) . collectAst) sources)
    exports = Map.fromList [(getName poly, getName mono) | source <- sources, ABE poly mono _ _ <- collectAst source]
    resolve seen name
        | Just body <- Map.lookup name bodies = Just (name, body)
        | Set.member name seen = Nothing
        | otherwise = Map.lookup name exports >>= resolve (Set.insert name seen)
    instantiate (_, name, ty) = do
        (key, body) <- resolve Set.empty name
        let actual = rho ty
        if firstOrder actual then (key,body,) <$> tcMatchTy (template body) actual else Nothing

    -- For a first-order value only its outer type constructor decides the
    -- role. Carry open heads across statically named calls, including aliases.
    -- The domain is finite: only free variables of each retained template can
    -- be added. Recursive calls never require unfolding a growing type or RHS.
    open body ty = do
        v <- getTyVar_maybe (fst (splitAppTys ty))
        if v `elemVarSet` tyCoVarsOfType (template body) then Just v else Nothing
    seeds = Map.map (\body -> Set.fromList (mapMaybe (open body) (forcedOperands body))) bodies
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
          let operand = quickCheckOperand result
              becomes ty = not (operand ty) && operand (substTy sub ty),
          any becomes (forcedOperands body) || any (becomes . mkTyVarTy) (heads Map.! key)]

rho :: Type -> Type
rho ty = let (_, _, body) = tcSplitSigmaTy ty in body

firstOrder :: Type -> Bool
firstOrder = all (null . fst . splitPiTys . scaledThing) . fst . splitFunTys

ordinaryBody :: QuickCheck -> HsBind GhcTc -> Maybe (Name, Body)
ordinaryBody qc binding = do
    identifier <- bindingId binding
    let record scheme rhs
            | firstOrder scheme, not (isQuickCheckBinding qc identifier) =
                let (observed, direct) = bodyUses qc rhs in Just (getName identifier, Body scheme observed direct)
            | otherwise = Nothing
    case binding of
        FunBind _ _ matches -> let MatchGroupTc args res _ = mg_ext matches in record (mkScaledFunTys args res) matches
        PatBind{pat_lhs = pat, pat_rhs = rhs} -> record (hsLPatType pat) rhs
        _ -> Nothing

-- A single variable pattern names its own RHS, just as a zero-argument
-- equation does. A field of a destructured value is not such an alias.
bindingId :: HsBind GhcTc -> Maybe Id
bindingId (FunBind _ n _) = Just (unLoc n)
bindingId PatBind{pat_lhs = pat} = variable (unLoc pat)
  where
    variable (VarPat _ n) = Just (unLoc n)
    variable (ParPat _ p) = variable (unLoc p)
    variable (SigPat _ p _) = variable (unLoc p)
    variable (BangPat _ p) = variable (unLoc p)
    variable (LazyPat _ p) = variable (unLoc p)
    variable (XPat (CoPat _ p _)) = variable p
    variable _ = Nothing
bindingId _ = Nothing

typedCall :: LHsExpr GhcTc -> Maybe Call
typedCall expr = do
    (location, name) <- typedVariable expr
    -- The direct-call rule concerns a written occurrence. A syntax expansion
    -- can also contain an unlocated compiler invocation (e.g. rebindable if).
    case getBufSpan location of
        Strict.Just _ -> pure (location, name, lhsExprType expr)
        Strict.Nothing -> Nothing

-- Retain force sites from every equation, even erased ones. The wired-in seq
-- forces its first operand; strict patterns and fields force their values.
-- A variable or selector can refer to a known callee, but returning its result
-- does not put that result in a force position in this body.
bodyUses :: Data a => QuickCheck -> a -> ([Type], [Call])
bodyUses qc = walk
  where
    walk :: Data a => a -> ([Type], [Call])
    walk node
        | Just binding <- cast node, Just n <- bindingId binding, isQuickCheckBinding qc n = ([], [])
        | otherwise = own <> mconcat (children walk node)
      where
        own | Just e <- cast node, Just call@(_, name, ty) <- typedCall e =
                  ([scaledThing first | name == getName seqId,
                      first:_ <- [fst (splitFunTys (rho ty))]], [call])
            | Just pat@(L _ BangPat{}) <- cast node :: Maybe (LPat GhcTc) = ([hsLPatType pat], [])
            | Just (FunBind _ _ matches) <- cast node :: Maybe (HsBind GhcTc), strictNullaryRhs matches =
                ([mg_res_ty (mg_ext matches)], [])
            | Just e <- cast node, Just (_, _, operands) <- strictConstructorOperands e = (operands, [])
            | Just (ExpandedThingTc (OrigExpr RecordUpd{rupd_flds = RegularRecUpdFields parents _}) expanded) <- cast node =
                (updatedStrictFields [greName f | parent <- toList parents, f <- toList (rnRecUpdLabels parent)] expanded, [])
            | otherwise = ([], [])

-- Updating a lazy field need not force any new value at an unchanged strict
-- slot. GHC's update cases choose the constructors; the written field labels
-- choose only the assignments. A type-changing update uses the result type.
updatedStrictFields :: [Name] -> HsExpr GhcTc -> [Type]
updatedStrictFields labels expanded =
    [substTy sub (scaledThing arg) | (RealDataCon con, resultTy) <- updateAlternatives expanded,
        Just sub <- [tcMatchTy (dataConOrigResTy con) resultTy],
        (field, bang, arg) <- zip3 (dataConFieldLabels con) (dataConImplBangs con) (dataConOrigArgTys con),
        flSelector field `elem` labels, isBanged bang]
