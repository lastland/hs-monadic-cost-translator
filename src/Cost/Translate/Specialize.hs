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
import Cost.Translate.Callable (spine, expressions)
import Cost.Translate.Sharing (constrainedVariables)
import Cost.Translate.Typed (FunctionType(..), expressionTypeAt)
import Cost.Translate.Validation (scalarType)
import Control.Monad (unless)
import Data.Data (Data, cast, gmapQ)
import Data.List (nubBy)
import Data.Maybe (maybeToList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data HelperReference
    = BareReference
    | PrefixApplication Int
    | InfixApplication Int

data HelperSpecialization = HelperSpecialization
    { specializedHelper :: Name
    , helperOwnedVariable :: TyVar
    , helperInstantiation :: Type
    , helperQualifiedType :: Type
    }

specializeHelpers :: HsGroup GhcRn -> Map.Map Name FunctionType -> Map.Map String Type
    -> (Map.Map Name Type, Type -> Type)
specializeHelpers group checked hints =
    (Map.fromList [(specializedHelper result, adjust (helperQualifiedType result)) | result <- planned], adjust)
  where
    bindings = collect group :: [LHsBind GhcRn]
    sourceExpressions = collect group :: [LHsExpr GhcRn]
    topLevel = Set.fromList [unLoc name | L _ (FunBind _ name _) <- case hs_valds group of
        XValBindsLR (NValBinds groups _) -> concatMap (bagToList . snd) groups
        ValBinds _ binds _ -> bagToList binds]
    recursive = Set.fromList [unLoc name |
        XValBindsLR (NValBinds groups _) <- collect group :: [HsValBinds GhcRn],
        (Recursive, binds) <- groups, L _ (FunBind _ name _) <- bagToList binds]
    singletonRecursive = Set.fromList [unLoc name |
        XValBindsLR (NValBinds groups _) <- collect group :: [HsValBinds GhcRn],
        (Recursive, binds) <- groups, [L _ (FunBind _ name matches)] <- [bagToList binds],
        arity matches > 0]
    candidates = [(unLoc name, arity matches, info) |
        L _ (FunBind _ name matches) <- bindings, arity matches > 0,
        unLoc name `Set.notMember` topLevel,
        unLoc name `Set.notMember` recursive || unLoc name `Set.member` singletonRecursive,
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
    uses = Map.fromListWith (++) [(unLoc name, [(location expression, useType expression)]) |
        expression@(L _ (HsVar _ name)) <- sourceExpressions]
    ownership = Map.fromListWith (++) (referenceOwners [] group)
    referenceOwners :: Data a => [(Name, SrcSpan)] -> a -> [((Name, String), [([(Name, SrcSpan)], SrcSpan)])]
    referenceOwners owners syntax = case cast syntax :: Maybe (LHsBind GhcRn) of
        Just binding@(L _ (FunBind _ name _)) ->
            concat (gmapQ (referenceOwners (owners ++ [(unLoc name, getLocA binding)])) (unLoc binding))
        _ -> case cast syntax :: Maybe (LHsExpr GhcRn) of
            Just expression@(L _ (HsVar _ name)) -> [((unLoc name, location expression), [(owners, getLocA expression)])]
            _ -> concat (gmapQ (referenceOwners owners) syntax)
    useType = expressionTypeAt sourceExpressions hints
    applications = [((unLoc name, location function), reference (length arguments)) |
        expression <- expressions group,
        (function, arguments, reference) <- case unLoc expression of
            HsApp {} -> let (function, arguments) = spine expression in [(function, arguments, PrefixApplication)]
            OpApp _ left operator right -> [(operator, [left, right], InfixApplication)]
            _ -> [],
        HsVar _ name <- [unLoc function]]
    heads = Set.fromList (map fst applications)
    calls = Map.fromListWith (++)
        ([(site, [application]) | (site, application) <- applications] ++
         [(site, [BareReference]) | expression@(L _ (HsVar _ name)) <- expressions group,
          let site = (unLoc name, location expression), site `Set.notMember` heads])
    planned = discover [] candidates
    adjust = substitute planned
    substitute plan ty = foldl (\current result ->
        substTyWith [helperOwnedVariable result] [helperInstantiation result] current) ty plan
    discover plan remaining = case [result | candidate <- remaining, Just result <- [specialization plan candidate]] of
        [] -> plan
        result : _ -> discover (plan ++ [result]) (filter (\(other, _, _) -> other /= specializedHelper result) remaining)
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
        unless (not (null references) && all (\(site, _) -> case Map.lookup (name, site) calls of
            Just [BareReference] -> True
            Just [PrefixApplication provided] -> provided > 0 && provided <= count
            Just [InfixApplication provided] -> provided == count
            _ -> False) references) Nothing
        actualTypes <- mapM (fmap normalize . snd) references
        outsideTypes <- if name `Set.member` recursive then do
            [binding] <- pure [binding' | binding'@(L _ (FunBind _ identifier _)) <- bindings, unLoc identifier == name]
            unless (real (getLocA binding)) Nothing
            classified <- mapM (\(site, hint) -> do
                [(owners, span')] <- Map.lookup (name, site) ownership
                unless (real span') Nothing
                let inside = name `elem` map fst owners
                unless (not inside || lookup name owners == Just (getLocA binding) &&
                    span' `isSubspanOf` getLocA binding) Nothing
                actualType <- normalize <$> hint
                unless (not inside || actualType `eqType` body) Nothing
                pure (inside, actualType)) references
            unless (any fst classified && not (all fst classified)) Nothing
            pure [actualType | (False, actualType) <- classified]
          else pure actualTypes
        [actualType] <- pure (nubBy eqType outsideTypes)
        actual <- match variable tau actualType
        unless (available scope actual) Nothing
        checkedOwned <- match variable tau body
        unless (checkedOwned `eqType` mkTyVarTy owned) Nothing
        unless (renderingSafe plan name actual) Nothing
        pure HelperSpecialization
            { specializedHelper = name
            , helperOwnedVariable = owned
            , helperInstantiation = actual
            , helperQualifiedType = substTyWith [variable] [actual] (snd (splitForAllTyVars ty))
            }
    renderingSafe plan name actual = all safe (nonDetEltsUniqSet (tyCoVarsOfType actual))
      where
        removed = Set.fromList (name : map specializedHelper plan)
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
    real RealSrcSpan {} = True
    real _ = False

arity :: MatchGroup GhcRn (LHsExpr GhcRn) -> Int
arity matches = case unLoc (mg_alts matches) of
    L _ match : _ -> length (m_pats match)
    [] -> 0

location :: LHsExpr GhcRn -> String
location = showSDocUnsafe . ppr . getLocA

collect :: (Data a, Data b) => b -> [a]
collect value = maybeToList (cast value) ++ concat (gmapQ collect value)
