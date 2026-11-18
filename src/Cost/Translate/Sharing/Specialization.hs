module Cost.Translate.Sharing.Specialization where

import Control.Monad (unless, zipWithM)
import Cost.Translate.Validation (scalarType)
import Data.List (nubBy)
import GHC (TyCon, TyVar, Type)
import GHC.Builtin.Types
    ( liftedTypeKind
    , listTyCon
    , maybeTyCon
    , tupleTyCon
    )
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.FVs (tyCoVarsOfType, tyCoVarsOfTypes)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type
    ( getTyVar_maybe
    , isPredTy
    , splitForAllTyVars
    , splitFunTys
    , splitTyConApp_maybe
    , substTyWith
    )
import GHC.Types.Basic (Boxity (..))
import GHC.Types.Var (tyVarKind)
import GHC.Types.Var.Set (elemVarSet, isEmptyVarSet)

constrainedVariables :: Type -> [TyVar]
constrainedVariables ty = [v | v <- vars, v `elemVarSet` free]
    where
        (vars, body) = splitForAllTyVars ty
        arguments = map scaledThing (fst (splitFunTys body))
        predicates = takeWhile isPredTy arguments
        free = tyCoVarsOfTypes predicates

specializedSharing ::
    [TyCon] -> [TyVar] -> [Type] -> Type -> [Maybe Type] -> Maybe [Type]
specializedSharing datatypes scopeVariables givens ty uses
    | length vars > 1
    , all ((`eqType` liftedTypeKind) . tyVarKind) vars
    , all (`elem` constrainedVariables ty) vars
    , all (isPredTy . scaledThing) arguments
    , not (null uses) = do
        types <- sequence uses
        let distinct = nubBy eqType types
        case distinct of
            [value]
                | not (isEmptyVarSet (tyCoVarsOfType value)) ->
                    scopedInstantiation matcher scopeVariables givens ty value
            _ -> mapM_ (closedInstantiation matcher vars result) types
        pure distinct
    | [variable] <- vars
    , tyVarKind variable `eqType` liftedTypeKind
    , variable `elem` constrainedVariables ty
    , all (isPredTy . scaledThing) arguments
    , Just resultVariable <- resultParameter result >>= getTyVar_maybe
    , resultVariable == variable
    , not (null uses) = do
        types <- sequence uses
        let distinct = nubBy eqType types
        if all closedResult types
            then Just distinct
            else case distinct of
                [value]
                    | Just scoped <- resultParameter value >>= getTyVar_maybe
                    , scoped `elem` scopeVariables ->
                        Just distinct
                _ -> Nothing
    | otherwise = Nothing
    where
        (vars, body) = splitForAllTyVars ty
        (arguments, result) = splitFunTys body
        matcher = parameters datatypes scopeVariables vars result
        resultParameter value = do
            distinctParameters <-
                nubBy eqType . map snd <$> matcher value
            case distinctParameters of
                [element] -> Just element
                _ -> Nothing
        closedResult value = maybe False closedScalar (resultParameter value)

closedScalar :: Type -> Bool
closedScalar value = isEmptyVarSet (tyCoVarsOfType value) && scalarType value

closedInstantiation ::
    (Type -> Maybe [(TyVar, Type)]) ->
    [TyVar] ->
    Type ->
    Type ->
    Maybe ()
closedInstantiation matcher vars result value = do
    matches <- matcher value
    replacements <- mapM (`lookup` matches) vars
    unless
        ( isEmptyVarSet (tyCoVarsOfType value)
            && all closedScalar replacements
            && substTyWith vars replacements result `eqType` value
        )
        Nothing

scopedInstantiation ::
    (Type -> Maybe [(TyVar, Type)]) ->
    [TyVar] ->
    [Type] ->
    Type ->
    Type ->
    Maybe ()
scopedInstantiation matcher scopeVariables givens ty value = do
    matches <- matcher value
    replacements <- mapM (`lookup` matches) vars
    let instantiated = map (substTyWith vars replacements) predicates
    unless
        ( all available replacements
            && substTyWith vars replacements result `eqType` value
            && all (\predicate -> any (eqType predicate) givens) instantiated
        )
        Nothing
    where
        (vars, body) = splitForAllTyVars ty
        (arguments, result) = splitFunTys body
        predicates = map scaledThing arguments
        available actual =
            closedScalar actual
                || maybe False (`elem` scopeVariables) (getTyVar_maybe actual)

parameters ::
    [TyCon] ->
    [TyVar] ->
    [TyVar] ->
    Type ->
    Type ->
    Maybe [(TyVar, Type)]
parameters datatypes scopeVariables vars shape value
    | Just variable <- getTyVar_maybe shape
    , variable `elem` vars =
        Just [(variable, value)]
    | Just captured <- getTyVar_maybe shape
    , captured `elem` scopeVariables
    , shape `eqType` value =
        Just []
    | isEmptyVarSet (tyCoVarsOfType shape), shape `eqType` value = Just []
    | Just (constructor, elements) <- splitTyConApp_maybe shape
    , constructor `elem` constructors
    , Just (actualConstructor, actualElements) <- splitTyConApp_maybe value
    , actualConstructor == constructor
    , length actualElements == length elements =
        concat <$> zipWithM descend elements actualElements
    | otherwise = Nothing
    where
        constructors = datatypes ++ [maybeTyCon, listTyCon, tupleTyCon Boxed 2]
        descend = parameters datatypes scopeVariables vars
