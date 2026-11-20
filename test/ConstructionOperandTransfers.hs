-- | Replay the ordinary callee's operand obligations, not the caller's role.
-- In particular a property can construct and observe its own generator, but
-- calling an ordinary home function does not turn that function into a property.
module ConstructionOperandTransfers (ordinaryOperandTransfers) where

import ConstructionAst (children, collectAst, pretty, typedVariable)
import ConstructionQuickCheckTypes (checkedHome)
import ConstructionRoles (QuickCheck, isQuickCheckBinding)
import ConstructionRuntime (describeName)
import Data.Data (Data, cast)
import Data.Foldable (toList)
import Data.List (nub, nubBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import GHC
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.DataCon (dataConImplBangs, dataConOrigArgTys, dataConOrigResTy, isBanged)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type (getTyVar_maybe, mkScaledFunTys, mkTyVarTy, splitAppTys, splitFunTys, splitPiTys, splitTyConApp_maybe, substTy)
import GHC.Core.Unify (tcMatchTy)
import qualified GHC.Data.Strict as Strict
import GHC.Hs.Syn.Type (hsExprType, hsLPatType, lhsExprType)
import GHC.Tc.Utils.TcType (tcSplitSigmaTy)
import GHC.Types.FieldLabel (flSelector)
import GHC.Types.Id.Make (seqId)
import GHC.Types.Name.Reader (greName)
import GHC.Types.SrcLoc (getBufSpan)
import GHC.Types.Var.Set (elemVarSet)

type Call = (SrcSpan, Name, Type)

-- The template contains the equation's skolems, shared with its operand
-- types. AbsBinds exports can use different quantified variables.
data Body = Body { definingHome :: Module, template :: Type, forcedOperands :: [Type], calls :: [Call] }

ordinaryOperandTransfers :: (Module -> Type -> Bool) -> QuickCheck -> [TypecheckedModule] -> [String]
ordinaryOperandTransfers result qc modules = concatMap report sources
  where
    sources = [(checkedHome m, tm_typechecked_source m) | m <- modules]
    bodies = Map.fromList [(n, body) | (home, source) <- sources, b <- collectAst source, Just (n, body) <- [bindingBody home qc b]]
    exports = Map.fromList [(getName poly, getName mono) | (_, source) <- sources, ABE poly mono _ _ <- collectAst source]
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
    -- A family head also needs its arguments. Intern only the family operands
    -- actually present in a retained signature or use. These and a template's
    -- free variable heads are a finite domain even for polymorphic recursion.
    families = Map.map (\body -> concatMap familyOperands (template body : forcedOperands body ++ [rho t | (_, _, t) <- calls body])) bodies
    open key body ty
        | Just v <- getTyVar_maybe (fst (splitAppTys ty)), v `elemVarSet` tyCoVarsOfType (template body) = Just (mkTyVarTy v)
        | Just (tc, _) <- splitTyConApp_maybe ty, isTypeFamilyTyCon tc, any (eqType ty) (families Map.! key) = Just ty
        | otherwise = Nothing
    seeds = Map.mapWithKey (\k body -> [(t, Nothing) | t <- mapMaybe (open k body) (forcedOperands body)]) bodies
    edges = Map.map (mapMaybe (\call@(_, n, _) -> (n,) <$> instantiate call) . calls) bodies
    close known =
        let step name body = nubBy same (known Map.! name ++
                [(t, Just (fromMaybe called origin)) | (called, (child, _, sub)) <- edges Map.! name,
                    (h, origin) <- known Map.! child, Just t <- [open name body (substTy sub h)]])
            next = Map.mapWithKey step bodies
        in if fmap length next == fmap length known then next else close next
    same (a, via) (b, other) = via == other && eqType a b
    heads = close (fmap (nubBy same) seeds)

    -- The occurrence is checked even inside a QC body. Report only an
    -- obligation that becomes QC at this instantiation. An already concrete
    -- operand is checked at its own ordinary declaration, not at every caller.
    report (home, source) = nub
        [pretty location ++ ": QuickCheck value used outside a generator or property: " ++ describeName name
            ++ " (operand in ordinary callee" ++ (if origin == name then "" else " " ++ describeName origin) ++ " at this instantiation)"
        | e <- collectAst source, Just call@(location, name, _) <- [typedCall e],
          Just (key, body, sub) <- [instantiate call],
           let becomes ty = not (result (definingHome body) ty) && result home (substTy sub ty),
           origin <- [name | any becomes (forcedOperands body)] ++
               [fromMaybe name via | (t, via) <- heads Map.! key, becomes t]]

familyOperands :: Type -> [Type]
familyOperands ty = case splitTyConApp_maybe ty of
    Just (tc, args) -> [ty | isTypeFamilyTyCon tc] ++ concatMap familyOperands args
    Nothing -> concatMap familyOperands (snd (splitAppTys ty))

rho :: Type -> Type
rho ty = let (_, _, body) = tcSplitSigmaTy ty in body

firstOrder :: Type -> Bool
firstOrder = all (null . fst . splitPiTys . scaledThing) . fst . splitFunTys

bindingBody :: Module -> QuickCheck -> HsBind GhcTc -> Maybe (Name, Body)
bindingBody home qc binding = do
    identifier <- bindingId binding
    let record scheme rhs
            | firstOrder scheme =
                let (observed, direct) = bodyUses qc rhs
                    -- A property may force its own operands. Its statically
                    -- named home calls still carry their own ordinary forces.
                    ordinary = if isQuickCheckBinding qc identifier then [] else observed
                in Just (getName identifier, Body home scheme ordinary direct)
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
            | Just e <- cast node, Strict.Just _ <- getBufSpan (getLocA e),
              Just con <- constructorHead (unLoc (e :: LHsExpr GhcTc)) = (strictFields con (lhsExprType e), [])
            | Just RecordCon{rcon_con = n, rcon_ext = actual} <- cast node :: Maybe (HsExpr GhcTc),
              Strict.Just _ <- getBufSpan (getLocA n), RealDataCon con <- unLoc n =
                (strictFields con (hsExprType actual), [])
            | Just (ExpandedThingTc (OrigExpr RecordUpd{rupd_flds = RegularRecUpdFields parents _}) expanded) <- cast node =
                (updatedStrictFields [greName f | parent <- toList parents, f <- toList (rnRecUpdLabels parent)] expanded, [])
            | otherwise = ([], [])

-- Use the actual constructor wrapper's instantiated argument types in
-- declaration order, for positional, partially applied and labelled construction.
strictFields :: DataCon -> Type -> [Type]
strictFields con ty = [scaledThing arg | (bang, arg) <- zip (dataConImplBangs con) (fst (splitFunTys (rho ty))), isBanged bang]

constructorHead :: HsExpr GhcTc -> Maybe DataCon
constructorHead (XExpr (ConLikeTc (RealDataCon con) _ _)) = Just con
constructorHead (XExpr (WrapExpr (HsWrap _ e))) = constructorHead e
constructorHead (HsAppType _ e _) = constructorHead (unLoc e)
constructorHead (HsPar _ e) = constructorHead (unLoc e)
constructorHead (ExprWithTySig _ e _) = constructorHead (unLoc e)
constructorHead _ = Nothing

-- Updating a lazy field need not force any new value at an unchanged strict
-- slot. GHC's update cases choose the constructors; the written field labels
-- choose only the assignments. A type-changing update uses the result type.
updatedStrictFields :: [Name] -> HsExpr GhcTc -> [Type]
updatedStrictFields labels = go
  where
    go (HsLet _ _ body) = go (unLoc body)
    go (XExpr (WrapExpr (HsWrap _ e))) = go e
    go (HsCase RecUpd _ matches) =
        [substTy sub (scaledThing arg) | L _ match <- unLoc (mg_alts matches),
            L _ ConPat{pat_con = L _ (RealDataCon con)} <- m_pats match,
            Just sub <- [tcMatchTy (dataConOrigResTy con) (mg_res_ty (mg_ext matches))],
            (field, bang, arg) <- zip3 (dataConFieldLabels con) (dataConImplBangs con) (dataConOrigArgTys con),
            flSelector field `elem` labels, isBanged bang]
    go _ = []
