-- | Closed schemas for the preexisting demand generators/observers. A certified
-- helper may be referenced only by another certified observation declaration.
module ConstructionDemand (demandSupport) where

import ConstructionAst
import ConstructionRuntime
import Control.Monad (forM)
import Data.List (nub)
import Data.Maybe (mapMaybe, isNothing)
import GHC
import GHC.Builtin.Types (intTy, liftedTypeKind, listTyConName, nilDataConName)
import GHC.Core.Class (classTyVars)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (piResultTys, splitForAllTyVars, splitFunTys, splitTyConApp_maybe, mkTyVarTy, tyVarKind)
import GHC.Tc.Types (tcg_tcs)
import GHC.Types.SourceText (il_value)
import GHC.Core.TyCo.Rep (scaledThing)

data Interface = Interface Name Name Name
data Schema = Schema
    { rt :: Runtime
    , constructors :: [Name]
    , qc :: [(String, Name)]
    , genType :: Name
    , interfaces :: [Interface]
    , genFields :: [Name]
    , shrinkFields :: [Name]
    , arbitraryFields :: [Name]
    }

demandSupport :: Runtime -> [Name] -> [TypecheckedModule] -> Ghc ([(SrcSpan, Name)], [Name])
demandSupport runtime' dataNames checked = do
    module' <- findModule (mkModuleName "Test.QuickCheck") Nothing
    operations <- forM ["liftArbitrary", "liftShrink", "resize", "arbitrary", "shrink", "sized", "oneof"] $ \label ->
        (label,) <$> resolveExport module' label
    gen <- resolveType module' "Gen"
    let classes = mapMaybe tyConClass_maybe [tc | m <- checked, tc <- tcg_tcs (fst (tm_internals_ m))]
        protocol = mapMaybe (demandInterface runtime' (getName gen)) classes
        schema = Schema runtime' dataNames operations (getName gen) protocol [] [] []
        sourceBindings = [binding | m <- checked, Just (group, _, _, _, _) <- [tm_renamed_source m],
            binding <- collectAst group :: [HsBind GhcRn]]
    candidates <- forM sourceBindings $ \binding -> case binding of
        FunBind _ name _ -> do
            thing <- lookupName (unLoc name)
            pure (binding, case thing of Just (AnId identifier) -> Just (idType identifier); _ -> Nothing)
        _ -> pure (binding, Nothing)
    let helpers match = [(getLocA name, unLoc name) | (binding@(FunBind _ name _), Just ty) <- candidates, match schema binding ty]
        generate = helpers exactGenField
        shrink = helpers exactShrinkField
        arbitrary = helpers exactArbitraryField
        complete = schema { genFields = map snd generate, shrinkFields = map snd shrink, arbitraryFields = map snd arbitrary }
        methods = [(getLocA name, unLoc name) | binding@(FunBind _ name _) <- sourceBindings,
            exactMethod complete (unLoc name) binding]
        aliases = [(getLocA name, unLoc name) | (binding@(FunBind _ name _), Just ty) <- candidates,
            exactAlias complete binding ty]
        restricted = nub (map snd (generate ++ shrink ++ arbitrary ++ aliases) ++
            concat [[generator, shrinker] | Interface _ generator shrinker <- protocol])
    pure (generate ++ shrink ++ arbitrary ++ methods ++ aliases, restricted)

-- Match the entire abstract interface by types and superclass, not its name.
-- Its implementation remains a caller/dictionary contract, not verified code
-- supplied by an arbitrary downstream consumer.
demandInterface :: Runtime -> Name -> Class -> Maybe Interface
demandInterface runtime' gen cls = case (classTyVars cls, classSCTheta cls, classMethods cls) of
    ([a], [super], [first, second]) | eqType (tyVarKind a) liftedTypeKind,
            Just (parent, [ty]) <- getClassPredTys_maybe super,
            getName parent == runtimeName runtime' "Memorist.Data.Interface.Demand" "Demand", eqType ty (mkTyVarTy a) ->
        case [(g, s) | (g, s) <- [(first, second), (second, first)], generator a g, shrinker a s] of
            [(g, s)] -> Just (Interface (getName cls) (getName g) (getName s))
            _ -> Nothing
    _ -> Nothing
  where
    method var identifier = case splitForAllTyVars (idType identifier) of
        ([_], _) -> Just (splitFunTys (piResultTys (idType identifier) [mkTyVarTy var]))
        _ -> Nothing
    generator var identifier = case method var identifier of
        Just ([dictionary, size, input], output) -> classArgument (scaledThing dictionary) == Just (getName cls) &&
            eqType (scaledThing size) intTy && eqType (scaledThing input) (mkTyVarTy var) &&
            unary gen output (demandOf runtime' (mkTyVarTy var))
        _ -> False
    shrinker var identifier = case method var identifier of
        Just ([dictionary, input, demand], output) -> classArgument (scaledThing dictionary) == Just (getName cls) &&
            eqType (scaledThing input) (mkTyVarTy var) && demandOf runtime' (mkTyVarTy var) (scaledThing demand) &&
            listOf (scaledThing demand) output
        _ -> False

classArgument :: Type -> Maybe Name
classArgument predicate = getName . fst <$> getClassPredTys_maybe predicate

unary :: Name -> Type -> (Type -> Bool) -> Bool
unary name ty valid = case splitTyConApp_maybe ty of
    Just (constructor, [argument]) -> getName constructor == name && valid argument
    _ -> False

demandOf :: Runtime -> Type -> Type -> Bool
demandOf runtime' input ty = unary (runtimeName runtime' "Memorist.Data.Interface.Demand" "Dem") ty (eqType input)

listOf :: Type -> Type -> Bool
listOf element ty = case splitTyConApp_maybe ty of
    Just (constructor, [argument]) -> getName constructor == listTyConName && eqType argument element
    _ -> False

helperType :: Schema -> Int -> Type -> Maybe ([Type], Type, Type)
helperType policy count ty = case splitForAllTyVars ty of
    ([a], body) | eqType (tyVarKind a) liftedTypeKind -> case splitFunTys body of
        (dictionary : arguments, output) | length arguments == count,
                Just cls <- classArgument (scaledThing dictionary),
                cls `elem` [name | Interface name _ _ <- interfaces policy] ->
            Just (map scaledThing arguments, output, mkTyVarTy a)
        _ -> Nothing
    _ -> Nothing

isThunk :: Schema -> Type -> Type -> Bool
isThunk policy element ty = unary (runtimeName (rt policy) "Memorist.Base.Thunk" "Th") ty (eqType element)

isDemandThunk :: Schema -> Type -> Type -> Bool
isDemandThunk policy element ty = unary (runtimeName (rt policy) "Memorist.Data.Interface.Demand" "ThD") ty (eqType element)

isGeneratedDemand :: Schema -> Type -> Type -> Bool
isGeneratedDemand policy element ty = unary (genType policy) ty (\thd ->
    unary (runtimeName (rt policy) "Memorist.Data.Interface.Demand" "ThD") thd (demandOf (rt policy) element))

exactGenField :: Schema -> HsBind GhcRn -> Type -> Bool
exactGenField policy binding ty = case (helperType policy 2 ty, equations binding) of
    (Just ([size, thunk], output, a), Just [([sizePat, thunkPat], body)])
        | eqType size intTy, isThunk policy a thunk, isGeneratedDemand policy a output,
          Just n <- plainPattern sizePat, Just t <- plainPattern thunkPat ->
            bounded policy n body $ \recurse -> case call (qcName policy "liftArbitrary") recurse of
                Just [inner] -> case call [g | Interface _ g _ <- interfaces policy] inner of
                    Just [budget, payload] -> smaller policy n budget && projected policy t payload
                    _ -> False
                _ -> False
    _ -> False

exactShrinkField :: Schema -> HsBind GhcRn -> Type -> Bool
exactShrinkField policy binding ty = case (helperType policy 2 ty, equations binding) of
    (Just ([thunk, demand], output, a), Just [([pat], body)])
        | isThunk policy a thunk, unary (runtimeName (rt policy) "Memorist.Data.Interface.Demand" "ThD") demand (demandOf (rt policy) a),
          listOf demand output, Just input <- plainPattern pat -> case call (qcName policy "liftShrink") body of
            Just [inner] -> case call [s | Interface _ _ s <- interfaces policy] inner of
                Just [payload] -> projected policy input payload
                _ -> False
            _ -> False
    _ -> False

exactArbitraryField :: Schema -> HsBind GhcRn -> Type -> Bool
exactArbitraryField policy binding ty = case (splitForAllTyVars ty, equations binding) of
    (([a], rest), Just [([pat], body)]) | Just n <- plainPattern pat -> case splitFunTys rest of
        ([dictionary, size], output) | eqType (scaledThing size) intTy,
                Just (cls, [argument]) <- getClassPredTys_maybe (scaledThing dictionary),
                eqType argument (mkTyVarTy a), classMethods cls `hasMethod` qcName policy "arbitrary",
                unary (genType policy) output (isDemandThunk policy (mkTyVarTy a)) ->
            bounded policy n body $ \recurse -> case call (qcName policy "liftArbitrary") recurse of
                Just [inner] -> case call (qcName policy "resize") inner of
                    Just [budget, generator] -> smaller policy n budget && variable generator `elem` map Just (qcName policy "arbitrary")
                    _ -> False
                _ -> False
        _ -> False
    _ -> False
  where hasMethod methods names = any ((`elem` names) . getName) methods

qcName :: Schema -> String -> [Name]
qcName policy label = [name | (key, name) <- qc policy, label == key]

ordinary :: Schema -> String -> [Name]
ordinary policy label = [name | (key, name) <- standardNames (rt policy), key == "Prelude." ++ label]

operator :: [Name] -> LHsExpr GhcRn -> Maybe (LHsExpr GhcRn, LHsExpr GhcRn)
operator names expression = case unLoc (fst (spine expression)) of
    OpApp _ left op right | variable op `elem` map Just names -> Just (left, right)
    _ -> Nothing

integer :: Integer -> LHsExpr GhcRn -> Bool
integer expected expression = case unLoc (fst (spine expression)) of
    HsOverLit _ (OverLit _ (HsIntegral literal)) -> il_value literal == expected
    _ -> False

smaller :: Schema -> Name -> LHsExpr GhcRn -> Bool
smaller policy n expression = case operator (ordinary policy "-") expression of
    Just (left, right) -> variable left == Just n && integer 2 right
    _ -> False

projected :: Schema -> Name -> LHsExpr GhcRn -> Bool
projected policy input expression = case call (roleNames (rt policy) Payload) expression of
    Just [value] -> variable value == Just input
    _ -> False

bounded :: Schema -> Name -> LHsExpr GhcRn -> (LHsExpr GhcRn -> Bool) -> Bool
bounded policy n expression recurse = case unLoc (fst (spine expression)) of
    HsIf _ condition base step | Just (left, right) <- operator (ordinary policy "<=") condition,
            variable left == Just n, integer 1 right,
            Just [skip] <- call (ordinary policy "pure") base,
            variable skip == uniqueRole (roleNames (rt policy) DemandData) "Memorist.Data.Interface.Demand.Skip" -> recurse step
    _ -> False

uniqueRole :: [Name] -> String -> Maybe Name
uniqueRole names label = case [name | name <- names, describeName name == label] of [name] -> Just name; _ -> Nothing

exactMethod :: Schema -> Name -> HsBind GhcRn -> Bool
exactMethod policy name binding
    | name `elem` [g | Interface _ g _ <- interfaces policy] = allCases genCase
    | name `elem` [s | Interface _ _ s <- interfaces policy] = allCases shrinkCase
    | roleOf (rt policy) name == Just DemandArbitraryMethod = case equations binding of
        Just [([], body)] -> variable body `elem` map Just (ordinary policy "pure")
        _ -> False
    | otherwise = False
  where
    allCases predicate = case equations binding of Just cases -> not (null cases) && all (uncurry predicate) cases; _ -> False
    genCase [L _ (WildPat _)] body = variable body `elem` map Just (ordinary policy "pure")
    genCase [budget, pattern'] body | Just (_, fieldNames) <- fields policy pattern',
            Just (constructor, generated) <- applicative policy body, constructor `elem` constructors policy,
            length fieldNames == length generated = and
        [case call (genFields policy) generator of
            Just [size, input] -> variable input == Just field && budgetFor policy budget (length fieldNames) size
            _ -> False | (field, generator) <- zip fieldNames generated]
    genCase _ _ = False
    shrinkCase [L _ (WildPat _), L _ (WildPat _)] body = emptyList body
    shrinkCase [input, demand] body | Just (_, fields') <- fields policy input,
            Just (constructor, demands) <- fields policy demand, length fields' == length demands,
            let pieces = appendPieces policy body, length pieces == length fields' = and
        [shrinkPiece policy constructor fields' demands index piece | (index, piece) <- zip [0..] pieces]
    shrinkCase _ _ = False

fields :: Schema -> LPat GhcRn -> Maybe (Name, [Name])
fields policy (L _ (ParPat _ pat)) = fields policy pat
fields policy (L _ (ConPat _ constructor (PrefixCon [] pats))) | unLoc constructor `elem` constructors policy =
    (unLoc constructor,) <$> mapM plainPattern pats
fields _ _ = Nothing

applicative :: Schema -> LHsExpr GhcRn -> Maybe (Name, [LHsExpr GhcRn])
applicative policy expression
    | Just [value] <- call (ordinary policy "pure") expression, Just name <- variable value,
      name `elem` constructors policy = Just (name, [])
    | Just (constructor, argument) <- operator (ordinary policy "<$>") expression,
      Just name <- variable constructor, name `elem` constructors policy = Just (name, [argument])
    | Just (left, argument) <- operator (ordinary policy "<*>") expression,
      Just (name, arguments) <- applicative policy left = Just (name, arguments ++ [argument])
    | otherwise = Nothing

budgetFor :: Schema -> LPat GhcRn -> Int -> LHsExpr GhcRn -> Bool
budgetFor policy pat count expression = case plainPattern pat of
    Just n -> variable expression == Just n || case operator (ordinary policy "div") expression of
        Just (input, divisor) -> variable input == Just n && integer (fromIntegral (max 1 count)) divisor
        _ -> False
    Nothing -> count == 0

emptyList :: LHsExpr GhcRn -> Bool
emptyList expression = variable expression == Just nilDataConName || case unLoc (fst (spine expression)) of
    ExplicitList _ [] -> True
    _ -> False

appendPieces :: Schema -> LHsExpr GhcRn -> [LHsExpr GhcRn]
appendPieces policy expression = case operator (ordinary policy "++") expression of
    Just (left, right) -> appendPieces policy left ++ appendPieces policy right
    Nothing -> [expression | not (emptyList expression)]

shrinkPiece :: Schema -> Name -> [Name] -> [Name] -> Int -> LHsExpr GhcRn -> Bool
shrinkPiece policy constructor inputs demands index expression = case unLoc (fst (spine expression)) of
    HsDo _ ListComp (L _ [L _ (BindStmt _ pat source), L _ (LastStmt _ body _ _)])
        | Just smallerName <- plainPattern pat, Just [input, demanded] <- call (shrinkFields policy) source,
          variable input == indexName inputs, variable demanded == indexName demands,
          Just values <- call [constructor] body, length values == length demands -> and
            [variable value == Just (if i == index then smallerName else demand) | (i, demand, value) <- zip3 [0..] demands values]
    _ -> False
  where indexName names = case drop index names of name : _ -> Just name; _ -> Nothing

exactAlias :: Schema -> HsBind GhcRn -> Type -> Bool
exactAlias policy binding ty = case (equations binding, splitFunTys (snd (splitForAllTyVars ty))) of
    (Just [([], body)], (arguments, output)) | Just name <- variable body,
            name `elem` concat [[g, s] | Interface _ g s <- interfaces policy],
            let ordinaryArgs = [scaledThing argument | argument <- arguments, isNothing (classArgument (scaledThing argument))] ->
        case ordinaryArgs of
            [size, _] | eqType size intTy -> unary (genType policy) output (const True)
            [_, demand] -> listOf demand output
            _ -> False
    _ -> False
