module Cost.Translate.Specialize (specializeHelpers) where

import GHC
import GHC.Builtin.Types (liftedTypeKind)
import GHC.Core.Type (getTyVar_maybe, mkTyVarTy, splitForAllTyVars, splitFunTys, substTyWith)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.TyCo.Subst (lookupTyVar)
import GHC.Core.Unify (tcMatchTy)
import GHC.Data.Bag (bagToList)
import GHC.Tc.Utils.TcType (tcSplitSigmaTy)
import GHC.Types.Basic (RecFlag(..))
import GHC.Types.Name (getOccString)
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import GHC.Types.Var (tyVarKind)
import GHC.Types.Var.Set (isEmptyVarSet)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import Cost.Translate.Callable (spine)
import Cost.Translate.Sharing (constrainedVariables)
import Cost.Translate.Typed (FunctionType(..))
import Cost.Translate.Validation (scalarType)
import Control.Monad (unless)
import Data.Data (Data, cast, gmapQ)
import Data.List (nubBy)
import Data.Maybe (maybeToList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

specializeHelpers :: HsGroup GhcRn -> Map.Map Name FunctionType -> Map.Map String Type
    -> (Map.Map Name Type, Type -> Type)
specializeHelpers group checked hints =
    (Map.fromList [(name, adjust qualified) | (name, _, _, qualified) <- planned], adjust)
  where
    bindings = collect group :: [LHsBind GhcRn]
    sourceExpressions = collect group :: [LHsExpr GhcRn]
    topLevel = Set.fromList [unLoc name | L _ (FunBind _ name _) <- case hs_valds group of
        XValBindsLR (NValBinds groups _) -> concatMap (bagToList . snd) groups
        ValBinds _ binds _ -> bagToList binds]
    recursive = Set.fromList [unLoc name |
        XValBindsLR (NValBinds groups _) <- collect group :: [HsValBinds GhcRn],
        (Recursive, binds) <- groups, L _ (FunBind _ name _) <- bagToList binds]
    candidates = [(unLoc name, arity matches, info) |
        L _ (FunBind _ name matches) <- bindings, arity matches > 0,
        unLoc name `Set.notMember` (topLevel `Set.union` recursive),
        Just info <- [Map.lookup (unLoc name) checked]]
    declarations = Map.fromList (scopes [] group)
    lexical = Map.fromList (paths [] group)
    paths :: Data a => [Name] -> a -> [(Name, [Name])]
    paths ancestors syntax = case cast syntax :: Maybe (LHsBind GhcRn) of
        Just binding@(L _ (FunBind _ name _)) ->
            (unLoc name, ancestors) : concat (gmapQ (paths (ancestors ++ [unLoc name])) (unLoc binding))
        _ -> concat (gmapQ (paths ancestors) syntax)
    scopes :: Data a => [TyVar] -> a -> [(Name, [TyVar])]
    scopes variables syntax = case cast syntax :: Maybe (LHsBind GhcRn) of
        Just binding@(L _ (FunBind _ name matches)) ->
            let owned = [variable | arity matches > 0, Just info <- [Map.lookup (unLoc name) checked],
                         variable <- functionVariables info]
            in (unLoc name, variables) : concat (gmapQ (scopes (variables ++ owned)) (unLoc binding))
        _ -> concat (gmapQ (scopes variables) syntax)
    locations = Map.fromListWith (+) [(location expression, 1 :: Int) | expression <- sourceExpressions]
    uses = Map.fromListWith (++) [(unLoc name, [(location expression, useType expression)]) |
        expression@(L _ (HsVar _ name)) <- sourceExpressions]
    useType expression = case getLocA expression of
        RealSrcSpan {} | Map.lookup (location expression) locations == Just 1 -> Map.lookup (location expression) hints
        _ -> Nothing
    calls = Map.fromListWith (++) [((unLoc name, location function), [length arguments]) |
        expression@(L _ HsApp {}) <- expressions group,
        let (function, arguments) = spine expression, HsVar _ name <- [unLoc function]]
    planned = discover [] candidates
    adjust = substitute planned
    substitute plan ty = foldl (\current (_, owned, actual, _) -> substTyWith [owned] [actual] current) ty plan
    discover plan remaining = case [result | candidate <- remaining, Just result <- [specialization plan candidate]] of
        [] -> plan
        result@(name, _, _, _) : _ -> discover (plan ++ [result]) (filter (\(other, _, _) -> other /= name) remaining)
    specialization plan (name, count, info) = do
        let normalize = substitute plan
            ty = normalize (functionDeclaredType info)
            (formal, _, tau) = tcSplitSigmaTy ty
            body = normalize (functionBodyType info)
            scope = Map.findWithDefault [] name declarations
            references = Map.findWithDefault [] name uses
        [variable] <- pure formal
        [owned] <- pure (functionVariables info)
        unless (tyVarKind variable `eqType` liftedTypeKind && tyVarKind owned `eqType` liftedTypeKind &&
            variable `elem` constrainedVariables ty && length (fst (splitFunTys tau)) == count) Nothing
        unless (all (\(other, otherInfo) -> other == name || owned `notElem` functionVariables otherInfo) (Map.toList checked)) Nothing
        unless (not (null references) && all (\(site, _) -> Map.lookup (name, site) calls == Just [count]) references) Nothing
        actualTypes <- mapM (fmap normalize . snd) references
        [actualType] <- pure (nubBy eqType actualTypes)
        actual <- match variable tau actualType
        unless (available scope actual) Nothing
        checkedOwned <- match variable tau body
        unless (checkedOwned `eqType` mkTyVarTy owned) Nothing
        unless (renderingSafe plan name actual) Nothing
        pure (name, owned, actual, substTyWith [variable] [actual] (snd (splitForAllTyVars ty)))
    renderingSafe plan name actual = all safe (nonDetEltsUniqSet (tyCoVarsOfType actual))
      where
        removed = Set.fromList (name : [selected | (selected, _, _, _) <- plan])
        emitted owner = [variable | owner `Set.notMember` removed,
            Just info <- [Map.lookup owner checked], variable <- functionVariables info]
        ancestorOwners = reverse (Map.findWithDefault [] name lexical)
        descendantOwners = [owner | (owner, parents) <- Map.toList lexical, name `elem` parents]
        known = all (`Map.member` checked) (ancestorOwners ++ descendantOwners)
        ancestors = concatMap emitted ancestorOwners
        descendants = concatMap emitted descendantOwners
        sameOccurrence left right = getOccString left == getOccString right
        safe target = known &&
            (case filter (sameOccurrence target) ancestors of
                [] -> True
                nearest : _ -> nearest == target) &&
            all (\variable -> variable == target || not (sameOccurrence target variable)) descendants
    match variable shape actual = do
        matched <- tcMatchTy shape actual
        replacement <- lookupTyVar matched variable
        unless (substTyWith [variable] [replacement] shape `eqType` actual) Nothing
        pure replacement
    available scope ty
        | isEmptyVarSet (tyCoVarsOfType ty) = scalarType ty
        | Just variable <- getTyVar_maybe ty = variable `elem` scope && tyVarKind variable `eqType` liftedTypeKind &&
            all (`elem` scope) (nonDetEltsUniqSet (tyCoVarsOfType ty))
        | otherwise = False

arity :: MatchGroup GhcRn (LHsExpr GhcRn) -> Int
arity matches = case unLoc (mg_alts matches) of
    L _ match : _ -> length (m_pats match)
    [] -> 0

location :: LHsExpr GhcRn -> String
location = showSDocUnsafe . ppr . getLocA

collect :: (Data a, Data b) => b -> [a]
collect value = maybeToList (cast value) ++ concat (gmapQ collect value)

expressions :: Data a => a -> [LHsExpr GhcRn]
expressions value = case cast value of
    Just expression -> expression : case unLoc expression of
        HsApp {} -> let (function, arguments) = spine expression in concatMap expressions (function : arguments)
        _ -> concat (gmapQ expressions (unLoc expression))
    Nothing -> concat (gmapQ expressions value)
