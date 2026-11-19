-- | Link a home Repr dictionary to its actual reprM implementation.
-- These are conditional certificates: every module body and retained source
-- site is checked, and every caller must supply the audited carrier.
module ConstructionHomeRepr (HomeReprs, homeReprs, selectedHome, methodCarrier, methodContext, contextSite) where

import ConstructionAst (collectAst)
import ConstructionRuntime
import Control.Monad (guard)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import GHC
import qualified GHC.Core as C
import GHC.Core.Class (classAllSelIds, classTyCon)
import GHC.Core.DataCon (dataConWorkId)
import GHC.Core.FVs (exprFreeVars)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.Type (isPredTy, mkTyVarTy, splitForAllTyVars, splitFunTys, substTyWith)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.TyCon (tyConSingleDataCon)
import GHC.Tc.Types.Evidence (HsWrapper(..))
import GHC.Types.Id (isDFunId)
import GHC.Types.Var (isId, isTyVar)
import GHC.Types.Var.Set (VarSet, emptyVarSet, elemVarSet, mkVarSet, subVarSet)

data Scheme = Scheme [TyVar] [Type] Type
-- Only this field, not its sibling abstract, may use the DFun's context.
data Scope = Scope Name SrcSpan VarSet [Name]
data HomeReprs = HomeReprs (Map.Map Name (Scheme, [Bool])) (Map.Map Name Scope)

homeReprs :: Runtime -> [(TypecheckedModule, [C.CoreBind])] -> HomeReprs
homeReprs rt programs = HomeReprs (Map.fromList (map fst entries)) (Map.fromList (map snd entries))
  where
    entries = [((getName owner, (scheme, used)), (getName carrier, scope)) |
        (checked, program) <- programs, let bindings = C.flattenBinds program, let sources = sourceFields rt checked,
        (owner, dictionary) <- bindings, Just (scheme, implementation) <- [homeField rt owner dictionary],
        (method, carrier, span') <- sources, implementation == method,
        Just (carrierClass, [_, _]) <- [getClassPredTys_maybe (idType carrier)],
        getName carrierClass == runtimeName rt "Memorist.Base.Core" "Memorist",
        Just rhs <- [lookup method bindings], Just (scope, used) <- [fieldScope scheme method span' rhs]]

-- Link context positions to the actual method's formal binders. An unused
-- position still needs resolved evidence, but cannot authorize a selector.
-- The raw and source passes also check sites erased from optimized Core.
fieldScope :: Scheme -> Id -> SrcSpan -> C.CoreExpr -> Maybe (Scope, [Bool])
fieldScope (Scheme variables context _) method span' rhs = do
    let (binders, body) = C.collectBinders rhs
        (params, rest) = splitAt (length variables) binders
        ds = take (length context) rest
    guard (length params == length variables && all isTyVar params)
    let instantiate = substTyWith variables (map mkTyVarTy params)
    guard (matchContextBinders ds (map instantiate context))
    let free = exprFreeVars body
        used = map (`elemVarSet` free) ds
        slots = [getName v | (v, True) <- zip ds used]
    pure (Scope (getName method) span' (mkVarSet params) slots, used)

-- Match the actual DFun telescope against the dictionary constructor and the
-- reprM field's type AND evidence applications. No lookup by instance shape.
homeField :: Runtime -> Id -> C.CoreExpr -> Maybe (Scheme, Id)
homeField rt owner dictionary = do
    guard (isDFunId owner && length vars == length actual && all isPredTy context)
    (cls, [ty]) <- getClassPredTys_maybe predicate
    guard (getName cls == runtimeName rt "Memorist.Data.Interface.Data" "Repr")
    let selectors = classAllSelIds cls
        (ds, body) = C.collectBinders afterTypes
    guard (matchContextBinders ds (map instantiate context))
    (C.Var worker, C.Type headTy : fields) <- pure (C.collectArgs body)
    guard (worker == dataConWorkId (tyConSingleDataCon (classTyCon cls)))
    guard (eqType headTy (instantiate ty) && length selectors == length fields)
    [field] <- pure [f | (sel, f) <- zip selectors fields, roleOf rt (getName sel) == Just ReprMethod]
    (C.Var method, supplied) <- pure (C.collectArgs field)
    let (typeArgs, evArgs) = splitAt (length actual) supplied
    guard (length typeArgs == length actual && and (zipWith sameType typeArgs actual))
    guard (length evArgs == length ds && and (zipWith sameVar evArgs ds))
    pure (Scheme vars context ty, method)
  where
    (vars, rho) = splitForAllTyVars (idType owner)
    (args, predicate) = splitFunTys rho
    context = map scaledThing args
    (binders, afterTypes) = C.collectTyBinders dictionary
    actual = map mkTyVarTy binders
    instantiate = substTyWith vars actual
    sameType (C.Type t) = eqType t
    sameType _ = const False
    sameVar (C.Var v) = (== v)
    sameVar _ = const False

matchContextBinders :: [Id] -> [Type] -> Bool
matchContextBinders ds predicates = length ds == length predicates && all isId ds &&
    and (zipWith eqType (map idType ds) predicates)

-- Link the field back to its retained source binder as well. Inlining or
-- aliases must never turn an ordinary generic helper into a method body.
sourceFields :: Runtime -> TypecheckedModule -> [(Id, Id, SrcSpan)]
sourceFields rt checked = [(poly, carrier, span') | ab <- collectAst (tm_typechecked_source checked),
    ABE poly _ _ _ <- abs_exports ab,
    FunBind (inner, _) name _ <- collectAst (abs_binds ab) :: [HsBind GhcTc], (location, span') <- locations, getLocA name == location,
    carrier <- wrapperVars inner]
  where
    wrapperVars (WpCompose a b) = wrapperVars a ++ wrapperVars b
    wrapperVars (WpEvLam v) = [v]
    wrapperVars _ = []
    locations = [(getLocA name, getLocA binding) | Just (group, _, _, _, _) <- [tm_renamed_source checked],
        inst <- collectAst group :: [ClsInstDecl GhcRn], binding@(L _ (FunBind _ name _)) <- toList (cid_binds inst),
        roleOf rt (unLoc name) == Just ReprMethod]

-- At a public call the actual type arguments must be closed. Inside a
-- certified field they can also depend on its instance parameters: the field
-- receives those from the same concrete DFun application. Local polymorphism
-- and other given dictionaries cannot supply such a substitution.
selectedHome :: HomeReprs -> Maybe Name -> Name -> [Type] -> Type -> Maybe [(Type, Bool)]
selectedHome (HomeReprs heads scopes) caller owner arguments ty = do
    (Scheme variables context headTy, used) <- Map.lookup owner heads
    guard (length variables == length arguments && all inScope arguments)
    let instantiate = substTyWith variables arguments
    guard (eqType (instantiate headTy) ty)
    pure (zip (map instantiate context) used)
  where
    scope = case caller >>= (`Map.lookup` scopes) of
        Just (Scope _ _ params _) -> params
        Nothing -> emptyVarSet
    inScope t = tyCoVarsOfType t `subVarSet` scope

methodCarrier :: HomeReprs -> Name -> Bool
methodCarrier (HomeReprs _ scopes) = (`Map.member` scopes)

contextSite :: HomeReprs -> Either Name SrcSpan -> Maybe Name
contextSite (HomeReprs _ scopes) site = case [carrier | (carrier, Scope method span' _ _) <- Map.toList scopes,
        either (== method) (`isSubspanOf` span') site] of
    [carrier] -> Just carrier
    _ -> Nothing

methodContext :: HomeReprs -> Maybe Name -> Name -> Bool
methodContext (HomeReprs _ scopes) caller ev = case caller >>= (`Map.lookup` scopes) of
    Just (Scope _ _ _ slots) -> ev `elem` slots
    Nothing -> False
