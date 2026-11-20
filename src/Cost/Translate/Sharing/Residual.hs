module Cost.Translate.Sharing.Residual
    ( Specialization (..)
    , planSpecializations
    ) where

import Control.Monad (guard)
import Cost.Translate.Kinds (visibleArguments)
import Cost.Translate.Sharing.Native (nativeSharing)
import Cost.Translate.Sharing.Schema (planNativeSchemas)
import Cost.Translate.Sharing.Specialization
    ( closedScalar
    , constrainedVariables
    , scalarClasses
    , scalarWitnessClasses
    , scopedGivenClasses
    , specializedSharing
    )
import Cost.Translate.Typed (AllocationScope (..), emptyAllocationScope)
import Data.Either (fromRight)
import Data.List (nubBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set qualified as Set
import GHC (TyCon, TyVar, Type, getName)
import GHC.Builtin.Names (numClassName)
import GHC.Builtin.Types (intTy, integerTy, liftedTypeKind)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.FVs (tyCoVarsOfType, tyCoVarsOfTypes)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.TyCo.Subst (lookupTyVar, substTy)
import GHC.Core.TyCon (isAlgTyCon)
import GHC.Core.Type
    ( getTyVar_maybe
    , isPredTy
    , mkForAllTys
    , mkTyVarTy
    , splitForAllForAllTyBinders
    , splitForAllTyVars
    , splitFunTys
    , splitTyConApp_maybe
    , substTyWith
    , typeKind
    )
import GHC.Core.Unify (tcMatchTy, typesCantMatch)
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import GHC.Types.Var (binderVar, isTyVar, tyVarKind)
import GHC.Types.Var.Set (elemVarSet, isEmptyVarSet)

data Specialization = Specialization
    { allocationType :: Type
    , useTypes :: [Type]
    , inferKeyFromUses :: Bool
    }

planSpecializations ::
    [TyCon] ->
    Maybe AllocationScope ->
    Type ->
    [Maybe Type] ->
    Maybe [Specialization]
planSpecializations datatypes evidence ty uses =
    case specializedSharing datatypes variables givens ty uses of
        Just types -> Just [Specialization value [value] False | value <- types]
        Nothing -> do
            scope <- evidence
            residualSharing datatypes scope ty uses
    where
        AllocationScope variables givens _ =
            fromMaybe emptyAllocationScope evidence

residualSharing ::
    [TyCon] ->
    AllocationScope ->
    Type ->
    [Maybe Type] ->
    Maybe [Specialization]
residualSharing datatypes evidence ty uses = do
    guard (not (null uses) && not (null constrained) && not (null residual))
    guard (all ((`eqType` liftedTypeKind) . tyVarKind) constrained)
    guard (all (isPredTy . scaledThing) arguments && available ty)
    actual <- sequence uses
    matches <- mapM (matchUse ty) actual
    keys <- mapM (\pairs -> mapM (`lookup` pairs) constrained) matches
    guard (all (all available) keys)
    let distinct = nubBy sameKey keys
    guard (separateKeys distinct)
    mapM (partition (inferKey distinct) keys actual) distinct
    where
        (binders, body) = splitForAllForAllTyBinders ty
        constrained = constrainedVariables ty
        residual = filter ((`notElem` constrained) . binderVar) binders
        (arguments, result) = splitFunTys body
        scope = tyCoVarsOfTypes (map mkTyVarTy (ownerVariables evidence))
        schemas = planNativeSchemas Set.empty datatypes
        available =
            all (`elemVarSet` scope) . nonDetEltsUniqSet . tyCoVarsOfType
        inferKey [[key]]
            | [formal] <- constrained
            , Just owner <- getTyVar_maybe key =
                isNothing (nameableDeclaredGivens evidence)
                    && isEmptyVarSet (tyCoVarsOfType ty)
                    && owner `elem` ownerVariables evidence
                    && visibleIn formal result
        inferKey _ = False
        partition infer keys actual key = do
            let instantiate = substTyWith constrained key
                partial = instantiate (mkForAllTys residual result)
                predicates = map (instantiate . scaledThing) arguments
            guard (all (predicateAvailable evidence infer) predicates)
            guard (not (any (`elemVarSet` tyCoVarsOfType partial) constrained))
            _ <-
                fromRight Nothing $
                    nativeSharing schemas Map.empty partial
            pure
                ( Specialization
                    partial
                    ( nubBy
                        eqType
                        [use | (k, use) <- zip keys actual, sameKey k key]
                    )
                    infer
                )

visibleIn :: TyVar -> Type -> Bool
visibleIn v ty = case splitTyConApp_maybe ty of
    Just (tc, xs) | isAlgTyCon tc -> any (visibleIn v) (visibleArguments tc xs)
    _ -> getTyVar_maybe ty == Just v

matchUse :: Type -> Type -> Maybe [(TyVar, Type)]
matchUse sigma use = do
    substitution <- tcMatchTy result use
    guard (all (unchanged substitution) captured)
    let pairs = [(v, substTy substitution (mkTyVarTy v)) | v <- quantified]
        instantiate = substTyWith quantified (map snd pairs)
    guard (instantiate result `eqType` use)
    guard
        ( all
            ( \(v, actual) ->
                isNothing (lookupTyVar substitution v)
                    || typeKind actual `eqType` instantiate (tyVarKind v)
            )
            pairs
        )
    pure pairs
    where
        (quantified, body) = splitForAllTyVars sigma
        (_, result) = splitFunTys body
        captured = nonDetEltsUniqSet (tyCoVarsOfType sigma)
        unchanged substitution v =
            isTyVar v
                && substTy substitution (mkTyVarTy v) `eqType` mkTyVarTy v
                && substTy substitution (tyVarKind v) `eqType` tyVarKind v

predicateAvailable :: AllocationScope -> Bool -> Type -> Bool
predicateAvailable evidence infer p = case splitTyConApp_maybe p of
    Just (constructor, [actual])
        | getName constructor == numClassName
        , actual `eqType` intTy || actual `eqType` integerTy ->
            True
        | getName constructor `elem` scalarClasses
        , closedScalar actual ->
            given
        | Just variable <- getTyVar_maybe actual
        , variable `elem` ownerVariables evidence
        , given ->
            supplied (getName constructor) actual
    _ -> False
    where
        given = any (eqType p) (checkedGivens evidence)
        context = nameableDeclaredGivens evidence
        supplied cls ty
            | cls == numClassName = True
            | cls `elem` scopedGivenClasses = infer || isJust context
            | cls `elem` scalarWitnessClasses =
                any (suppliesScalar ty) (fromMaybe [] context)
            | otherwise = False
        suppliesScalar ty predicate = case splitTyConApp_maybe predicate of
            Just (c, [arg]) ->
                getName c `elem` scalarWitnessClasses && eqType ty arg
            _ -> False

sameKey :: [Type] -> [Type] -> Bool
sameKey left right =
    length left == length right && and (zipWith eqType left right)

separateKeys :: [[Type]] -> Bool
separateKeys [] = True
separateKeys (key : rest) =
    all (typesCantMatch . zip key) rest && separateKeys rest
