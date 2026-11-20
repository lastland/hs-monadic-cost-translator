module Cost.Translate.Sharing.Specialization where

import Control.Applicative ((<|>))
import Control.Monad (guard, unless, zipWithM)
import Cost.Translate.Datatypes (renderType)
import Cost.Translate.Kinds (renderKind)
import Cost.Translate.Validation (scalarType)
import Data.List (nubBy)
import GHC (TyCon, TyVar, Type)
import GHC.Builtin.Names qualified as Builtin
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
    , splitAppTy_maybe
    , splitForAllTyVars
    , splitFunTys
    , splitTyConApp_maybe
    , substTyWith
    , typeKind
    )
import GHC.Core.Unify (typesCantMatch)
import GHC.Types.Basic (Boxity (..))
import GHC.Types.Name (Name, getName)
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
specializedSharing datatypes scopeVariables givens ty uses =
    closedSharing
        datatypes
        (matchParameters True datatypes scopeVariables vars result)
        ty
        uses
        <|> genericSharing datatypes scopeVariables givens ty uses
    where
        (vars, body) = splitForAllTyVars ty
        result = snd (splitFunTys body)

genericSharing ::
    [TyCon] -> [TyVar] -> [Type] -> Type -> [Maybe Type] -> Maybe [Type]
genericSharing datatypes scopeVariables givens ty uses
    | length vars > 1
    , all ((`eqType` liftedTypeKind) . tyVarKind) vars
    , all (`elem` constrainedVariables ty) vars
    , all (isPredTy . scaledThing) arguments
    , not (null uses) = do
        types <- sequence uses
        let distinct = nubBy eqType types
        unless
            ( all (isEmptyVarSet . tyCoVarsOfType) distinct
                || pairwiseDisjoint distinct
            )
            Nothing
        mapM_ instantiate distinct
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
        instantiate value
            | isEmptyVarSet (tyCoVarsOfType value) =
                closedInstantiation matcher vars result value
            | otherwise =
                scopedInstantiation matcher scopeVariables givens ty value
        resultParameter value = do
            distinctParameters <-
                nubBy eqType . map snd <$> matcher value
            case distinctParameters of
                [element] -> Just element
                _ -> Nothing
        closedResult value = maybe False closedScalar (resultParameter value)

closedSharing ::
    [TyCon] ->
    (Type -> Maybe [(TyVar, Type)]) ->
    Type ->
    [Maybe Type] ->
    Maybe [Type]
closedSharing datatypes matcher ty uses = do
    guard (not (null vars) && not (null uses))
    guard (all (`elem` constrainedVariables ty) vars)
    guard (all isPredTy predicates)
    types <- sequence uses
    let distinct = nubBy eqType types
    mapM_ instantiate distinct
    pure distinct
    where
        (vars, body) = splitForAllTyVars ty
        (arguments, result) = splitFunTys body
        predicates = map scaledThing arguments
        instantiate value = do
            guard (isEmptyVarSet (tyCoVarsOfType value))
            matches <- matcher value
            replacements <- mapM (`lookup` matches) vars
            let substitute = substTyWith vars replacements
            guard (substitute result `eqType` value)
            mapM_ (replacement substitute) (zip vars replacements)
            guard (all (targetDictionary . substitute) predicates)
            representable value
        replacement substitute (variable, actual) = do
            guard (isEmptyVarSet (tyCoVarsOfType actual))
            guard (typeKind actual `eqType` substitute (tyVarKind variable))
            either (const Nothing) (const (Just ())) $
                renderKind datatypes (typeKind actual)
            representable actual
        representable value =
            either (const Nothing) (const (Just ())) $
                renderType datatypes value

targetDictionary :: Type -> Bool
targetDictionary predicate
    | Just (constructor, [argument]) <- splitTyConApp_maybe predicate =
        if getName constructor == Builtin.foldableClassName
            then foldableDictionary argument
            else
                getName constructor `elem` scalarClasses
                    && closedScalar argument
    | otherwise = False

foldableDictionary :: Type -> Bool
foldableDictionary argument = case splitTyConApp_maybe argument of
    Just (constructor, []) -> constructor `elem` [listTyCon, maybeTyCon]
    Just (constructor, [_]) -> constructor == tupleTyCon Boxed 2
    _ -> False

scalarClasses :: [Name]
scalarClasses = scopedGivenClasses ++ scalarWitnessClasses

scopedGivenClasses :: [Name]
scopedGivenClasses =
    [ Builtin.numClassName
    , Builtin.fractionalClassName
    , Builtin.floatingClassName
    , Builtin.integralClassName
    , Builtin.realClassName
    , Builtin.realFracClassName
    , Builtin.realFloatClassName
    , Builtin.enumClassName
    , Builtin.boundedClassName
    ]

scalarWitnessClasses :: [Name]
scalarWitnessClasses = [Builtin.eqClassName, Builtin.ordClassName]

pairwiseDisjoint :: [Type] -> Bool
pairwiseDisjoint [] = True
pairwiseDisjoint (value : rest) =
    all (\other -> typesCantMatch [(value, other)]) rest
        && pairwiseDisjoint rest

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
parameters = matchParameters False

matchParameters ::
    Bool ->
    [TyCon] ->
    [TyVar] ->
    [TyVar] ->
    Type ->
    Type ->
    Maybe [(TyVar, Type)]
matchParameters applications datatypes scopeVariables vars shape value
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
    | applications
    , Just (function, argument) <- splitAppTy_maybe shape
    , Just (actualFunction, actualArgument) <- splitAppTy_maybe value = do
        functions <- descend function actualFunction
        arguments <- descend argument actualArgument
        pure (functions ++ arguments)
    | otherwise = Nothing
    where
        constructors = datatypes ++ [maybeTyCon, listTyCon, tupleTyCon Boxed 2]
        descend = matchParameters applications datatypes scopeVariables vars
