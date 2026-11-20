-- | Link a home Repr dictionary to its actual reprM implementation.
-- These are conditional certificates: every module body and retained source
-- site is checked, and every caller must supply the audited carrier.
module ConstructionHomeRepr (HomeReprs, homeReprs, selectedHome, methodCarrier, methodContext, contextSite, contextDeclaration, dictionaryBindings, withDictionaries, Field(..)) where

import ConstructionAst (collectAst, checkedModule)
import ConstructionRuntime
import Control.Monad (guard)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified GHC.Data.Strict as Strict
import GHC
import qualified GHC.Core as C
import GHC.Core.Class (classAllSelIds, classTyCon)
import GHC.Core.DataCon (dataConWorkId)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.Type (isPredTy, mkTyVarTy, splitForAllTyVars, splitFunTys, substTyWith)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.TyCon (tyConSingleDataCon)
import GHC.Tc.Types.Evidence (HsWrapper(..))
import GHC.Types.Id (isDFunId)
import GHC.Types.Var (isId, isTyVar)
import GHC.Types.Var.Set (emptyVarSet, mkVarSet, subVarSet)
import GHC.Types.SrcLoc (BufSpan, getBufSpan)

data Scheme = Scheme [TyVar] [Type] Type
-- Only this field, not its sibling abstract, may use the DFun's context.
data Field = Field { implementation :: Name, parameters :: [TyVar], slots :: [[Name]], carrierBinder :: Id }
data Scope = Scope Field Module SrcSpan
data HomeReprs = HomeReprs (Map.Map Name (Scheme, Field)) (Map.Map Name Scope) (Map.Map Name C.CoreExpr)

homeReprs :: Runtime -> [(TypecheckedModule, [C.CoreBind])] -> HomeReprs
homeReprs rt programs = HomeReprs (Map.fromList (map fst entries)) (Map.fromList (map snd entries)) dictionaries
  where
    -- A packed superclass may name a floated dictionary instead of its DFun.
    dictionaries = Map.fromList [(getName v, rhs) | (_, program) <- programs, (v, rhs) <- C.flattenBinds program,
        isDFunId v || isPredTy (idType v)]
    entries = [((getName owner, (scheme, field)), (getName carrier, Scope field (checkedModule checked) span')) |
        (checked, program) <- programs, let bindings = C.flattenBinds program, let sources = sourceFields rt checked,
        (owner, dictionary) <- bindings, Just (scheme, method) <- [homeField rt owner dictionary],
        (source, sourceSlots, carrier, span') <- sources, source == method,
        Just (carrierClass, [_, _]) <- [getClassPredTys_maybe (idType carrier)],
        getName carrierClass == runtimeName rt "Memorist.Base.Core" "Memorist",
        Just rhs <- [lookup method bindings], Just field <- [fieldScope scheme method sourceSlots carrier rhs]]

-- Match formal slots to both retained and desugared binders, even if the
-- optimizer has erased every use of the original dictionary.
fieldScope :: Scheme -> Id -> [Id] -> Id -> C.CoreExpr -> Maybe Field
fieldScope (Scheme variables context _) method sourceSlots carrier rhs = do
    let (params, rest) = splitAt (length variables) (fst (C.collectBinders rhs))
        ds = take (length context) rest
    guard (length params == length variables && all isTyVar params)
    let instantiate = substTyWith variables (map mkTyVarTy params)
        predicates = map instantiate context
    guard (matchContextBinders ds predicates && matchContextBinders sourceSlots predicates)
    pure (Field (getName method) params (zipWith (\a b -> [getName a, getName b]) ds sourceSlots) carrier)

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
sourceFields :: Runtime -> TypecheckedModule -> [(Id, [Id], Id, SrcSpan)]
sourceFields rt checked = [(poly, abs_ev_vars ab, carrier, span') | ab <- collectAst (tm_typechecked_source checked),
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
selectedHome :: HomeReprs -> Maybe Name -> Name -> [Type] -> Type -> Maybe (Field, [Type])
selectedHome (HomeReprs heads scopes _) lexicalField owner arguments ty = do
    (Scheme variables context headTy, field) <- Map.lookup owner heads
    guard (length variables == length arguments && all inScope arguments)
    let instantiate = substTyWith variables arguments
    guard (eqType (instantiate headTy) ty)
    pure (field, map instantiate context)
  where
    scope = case lexicalField >>= (`Map.lookup` scopes) of
        Just (Scope field _ _) -> mkVarSet (parameters field)
        Nothing -> emptyVarSet
    inScope t = tyCoVarsOfType t `subVarSet` scope

methodCarrier :: HomeReprs -> Name -> Bool
methodCarrier (HomeReprs _ scopes _) = (`Map.member` scopes)

contextSite :: HomeReprs -> Either Name SrcSpan -> Maybe Name
contextSite (HomeReprs _ scopes _) site = case [carrier | (carrier, Scope field _ span') <- Map.toList scopes,
        either (== implementation field) (`isSubspanOf` span') site] of
    [carrier] -> Just carrier
    _ -> Nothing

methodContext :: HomeReprs -> Maybe Name -> Name -> Bool
methodContext (HomeReprs _ scopes _) caller ev = case caller >>= (`Map.lookup` scopes) of
    Just (Scope field _ _) -> any (ev `elem`) (slots field)
    Nothing -> False

contextDeclaration :: HomeReprs -> Maybe (Module, BufSpan) -> Maybe Name
contextDeclaration (HomeReprs _ scopes _) (Just (owner, region)) =
    case [carrier | (carrier, Scope _ home span') <- Map.toList scopes,
        home == owner, getBufSpan span' == Strict.Just region] of
            [carrier] -> Just carrier
            _ -> Nothing
contextDeclaration _ _ = Nothing

dictionaryBindings :: HomeReprs -> Map.Map Name C.CoreExpr
dictionaryBindings (HomeReprs _ _ dictionaries) = dictionaries

withDictionaries :: HomeReprs -> Map.Map Name C.CoreExpr -> HomeReprs
withDictionaries (HomeReprs heads scopes dictionaries) retained =
    HomeReprs heads scopes (Map.union retained dictionaries)
