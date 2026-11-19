{-# LANGUAGE RecordWildCards #-}

module Cost.Translate.Specialize.Selection where

import Control.Monad (unless)
import Cost.Translate.Sharing (constrainedVariables)
import Cost.Translate.Specialize.Analysis
    ( Analysis (..)
    , HelperReference
        ( BareReference
        , InfixApplication
        , PrefixApplication
        )
    , HelperSpecialization (..)
    , LexicalOwner (ownerName, ownerSpan)
    , ReferenceOwnership (referenceAncestors, referenceSpan)
    , ReferenceSite
    )
import Cost.Translate.Typed
    ( FunctionType
        ( functionBodyType
        , functionDeclaredType
        , functionVariables
        )
    , SourceKey
    )
import Cost.Translate.Validation (scalarType)
import Data.List (find, nubBy)
import Data.Map.Strict qualified as Map
    ( Map
    , findWithDefault
    , lookup
    , member
    , toList
    )
import Data.Maybe (isJust)
import Data.Set qualified as Set (fromList, member, notMember)
import GHC
    ( GenLocated (L)
    , GhcRn
    , HsBindLR (FunBind)
    , LHsBind
    , Name
    , SrcSpan (RealSrcSpan)
    , TyVar
    , Type
    , getLocA
    , isSubspanOf
    , unLoc
    )
import GHC.Builtin.Types (liftedTypeKind)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.TyCo.Subst (lookupTyVar)
import GHC.Core.Type
    ( getTyVar_maybe
    , mkTyVarTy
    , splitForAllTyVars
    , splitFunTys
    , substTyWith
    )
import GHC.Core.Unify (tcMatchTy)
import GHC.Tc.Utils.TcType (tcSplitSigmaTy)
import GHC.Types.Name (getOccString)
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import GHC.Types.Var (tyVarKind)
import GHC.Types.Var.Set (isEmptyVarSet)

substitute :: [HelperSpecialization] -> Type -> Type
substitute plan ty =
    foldl
        ( \current result ->
            let owned = helperOwnedVariable result
                actual = helperInstantiation result
             in substTyWith [owned] [actual] current
        )
        ty
        plan

specialization ::
    Analysis ->
    [HelperSpecialization] ->
    (Name, Int, FunctionType) ->
    Maybe HelperSpecialization
specialization context@Analysis {..} plan (name, count, info) = do
    let normalize = substitute plan
        ty = normalize (functionDeclaredType info)
        (formal, _, tau) = tcSplitSigmaTy ty
        body = normalize (functionBodyType info)
        scope = Map.findWithDefault [] name declarations
        references = Map.findWithDefault [] name uses
    [variable] <- pure formal
    [owned] <- pure (functionVariables info)
    validateCandidate context name count ty tau variable owned references
    normalizedReferences <-
        mapM
            ( \(site, hint) -> do
                actualType <- normalize <$> hint
                pure (site, actualType)
            )
            references
    outsideTypes <- outsideReferences context name body normalizedReferences
    [actualType] <- pure (nubBy eqType outsideTypes)
    actual <- match variable tau actualType
    unless (available scope actual) Nothing
    checkedOwned <- match variable tau body
    unless (checkedOwned `eqType` mkTyVarTy owned) Nothing
    unless (renderingSafe context plan name actual) Nothing
    pure
        HelperSpecialization
            { specializedHelper = name
            , helperOwnedVariable = owned
            , helperInstantiation = actual
            , helperQualifiedType =
                substTyWith [variable] [actual] (snd (splitForAllTyVars ty))
            }
renderingSafe :: Analysis -> [HelperSpecialization] -> Name -> Type -> Bool
renderingSafe Analysis {..} plan name actual = all safe variables
    where
        removed = Set.fromList (name : map specializedHelper plan)
        emitted owner =
            [ variable
            | owner `Set.notMember` removed
            , Just info <- [Map.lookup owner checked]
            , variable <- functionVariables info
            ]
        ancestorOwners = reverse (Map.findWithDefault [] name lexical)
        variables = nonDetEltsUniqSet (tyCoVarsOfType actual)
        descendantOwners =
            [o | (o, parents) <- Map.toList lexical, name `elem` parents]
        known = all (`Map.member` checked) (ancestorOwners ++ descendantOwners)
        ancestors = concatMap emitted ancestorOwners
        descendants = concatMap emitted descendantOwners
        sameOccurrence left right = getOccString left == getOccString right
        safe target =
            known
                && ( case filter (sameOccurrence target) ancestors of
                        [] -> True
                        nearest : _ -> nearest == target
                   )
                && all
                    (\v -> v == target || not (sameOccurrence target v))
                    descendants

outsideReferences ::
    Analysis ->
    Name ->
    Type ->
    [(SourceKey, Type)] ->
    Maybe [Type]
outsideReferences Analysis {..} name body normalizedReferences =
    if name `Set.member` recursive
        then do
            [binding] <-
                pure
                    [ binding'
                    | binding'@(L _ (FunBind _ identifier _)) <- bindings
                    , unLoc identifier == name
                    ]
            unless (real (getLocA binding)) Nothing
            classified <-
                mapM
                    (classifyReference ownership name body binding)
                    normalizedReferences
            unless (any fst classified && not (all fst classified)) Nothing
            pure [actualType | (False, actualType) <- classified]
        else pure (map snd normalizedReferences)

classifyReference ::
    Map.Map ReferenceSite [ReferenceOwnership] ->
    Name ->
    Type ->
    LHsBind GhcRn ->
    (SourceKey, Type) ->
    Maybe (Bool, Type)
classifyReference ownership name body binding (site, actualType) = do
    [reference] <- Map.lookup (name, site) ownership
    let span' = referenceSpan reference
    unless (real span') Nothing
    let owner = find ((== name) . ownerName) (referenceAncestors reference)
        inside = isJust owner
    unless
        ( not inside
            || (ownerSpan <$> owner) == Just (getLocA binding)
                && span' `isSubspanOf` getLocA binding
        )
        Nothing
    unless (not inside || actualType `eqType` body) Nothing
    pure (inside, actualType)

match :: TyVar -> Type -> Type -> Maybe Type
match variable shape actual = do
    matched <- tcMatchTy shape actual
    replacement <- lookupTyVar matched variable
    unless (substTyWith [variable] [replacement] shape `eqType` actual) Nothing
    pure replacement
available :: [TyVar] -> Type -> Bool
available scope ty
    | isEmptyVarSet (tyCoVarsOfType ty) = scalarType ty
    | Just variable <- getTyVar_maybe ty =
        variable `elem` scope
            && tyVarKind variable `eqType` liftedTypeKind
            && all (`elem` scope) (nonDetEltsUniqSet (tyCoVarsOfType ty))
    | otherwise = False
real :: SrcSpan -> Bool
real RealSrcSpan {} = True
real _ = False

validateCandidate ::
    Analysis ->
    Name ->
    Int ->
    Type ->
    Type ->
    TyVar ->
    TyVar ->
    [(SourceKey, Maybe Type)] ->
    Maybe ()
validateCandidate Analysis {..} name count ty tau variable owned references = do
    unless
        ( tyVarKind variable `eqType` liftedTypeKind
            && tyVarKind owned `eqType` liftedTypeKind
            && variable `elem` constrainedVariables ty
            && length (fst (splitFunTys tau)) == count
        )
        Nothing
    unless
        ( all
            (\(key, i) -> key == name || owned `notElem` functionVariables i)
            (Map.toList checked)
        )
        Nothing
    unless
        ( not (null references)
            && all
                ( \(site, _) -> case Map.lookup (name, site) calls of
                    Just [BareReference] -> True
                    Just [PrefixApplication n] -> n > 0 && n <= count
                    Just [InfixApplication provided] -> provided == count
                    _ -> False
                )
                references
        )
        Nothing
