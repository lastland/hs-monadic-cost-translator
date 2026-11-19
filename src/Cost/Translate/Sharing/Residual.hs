module Cost.Translate.Sharing.Residual
    ( AllocationScope (..)
    , Specialization (..)
    , planSpecializations
    ) where

import Control.Monad (guard)
import Cost.Translate.Sharing.Native (nativeSharing)
import Cost.Translate.Sharing.Schema (planNativeSchemas)
import Cost.Translate.Sharing.Specialization
    ( closedScalar
    , constrainedVariables
    , scalarClasses
    , specializedSharing
    )
import Data.Either (fromRight)
import Data.List (nubBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Set qualified as Set
import GHC (Name, TyCon, TyVar, Type, getName)
import GHC.Builtin.Names (eqClassName, numClassName, ordClassName)
import GHC.Builtin.Types (intTy, integerTy, liftedTypeKind)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.FVs (tyCoVarsOfType, tyCoVarsOfTypes)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.TyCo.Subst (lookupTyVar, substTy)
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
import GHC.Types.Var.Set (elemVarSet)

data Specialization = Specialization
    { allocationType :: Type
    , useTypes :: [Type]
    }

data AllocationScope = AllocationScope
    { ownerVariables :: [TyVar]
    , checkedPredicates :: [Type]
    , namedContext :: Maybe [Type]
    }

planSpecializations ::
    [TyCon] ->
    Maybe AllocationScope ->
    Type ->
    [Maybe Type] ->
    Maybe [Specialization]
planSpecializations datatypes evidence ty uses =
    case specializedSharing datatypes variables givens ty uses of
        Just types -> Just [Specialization value [value] | value <- types]
        Nothing -> do
            scope <- evidence
            residualSharing datatypes scope ty uses
    where
        AllocationScope variables givens _ =
            fromMaybe (AllocationScope [] [] Nothing) evidence

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
    mapM (partition keys actual) distinct
    where
        (binders, body) = splitForAllForAllTyBinders ty
        constrained = constrainedVariables ty
        residual = filter ((`notElem` constrained) . binderVar) binders
        (arguments, result) = splitFunTys body
        scope = tyCoVarsOfTypes (map mkTyVarTy (ownerVariables evidence))
        schemas = planNativeSchemas Set.empty datatypes
        available =
            all (`elemVarSet` scope) . nonDetEltsUniqSet . tyCoVarsOfType
        partition keys actual key = do
            let instantiate = substTyWith constrained key
                partial = instantiate (mkForAllTys residual result)
                predicates = map (instantiate . scaledThing) arguments
            guard (all (predicateAvailable evidence) predicates)
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
                )

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

predicateAvailable :: AllocationScope -> Type -> Bool
predicateAvailable evidence predicate = case splitTyConApp_maybe predicate of
    Just (constructor, [actual])
        | getName constructor == numClassName
        , actual `eqType` intTy || actual `eqType` integerTy ->
            True
        | getName constructor `elem` scalarClasses
        , closedScalar actual ->
            given
        | getName constructor `elem` scalarClasses
        , Just variable <- getTyVar_maybe actual
        , variable `elem` ownerVariables evidence
        , given ->
            getName constructor == numClassName
                || maybe
                    False
                    (targetContext (getName constructor) actual)
                    (namedContext evidence)
    _ -> False
    where
        given = any (eqType predicate) (checkedPredicates evidence)

targetContext :: Name -> Type -> [Type] -> Bool
targetContext cls actual predicates =
    cls `notElem` comparisons || any suppliesScalar predicates
    where
        comparisons = [eqClassName, ordClassName]
        suppliesScalar p = case splitTyConApp_maybe p of
            Just (c, [ty]) -> getName c `elem` comparisons && eqType actual ty
            _ -> False

sameKey :: [Type] -> [Type] -> Bool
sameKey left right =
    length left == length right && and (zipWith eqType left right)

separateKeys :: [[Type]] -> Bool
separateKeys [] = True
separateKeys (key : rest) =
    all (typesCantMatch . zip key) rest && separateKeys rest
