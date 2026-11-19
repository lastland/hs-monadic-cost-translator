-- | A metadata view is not an arbitrary (a -> b). Both its declaration and
-- every retained source call need a closed structural certificate. Exporting
-- a checked helper does not certify future external consumers (policy #7).
module ConstructionViews (ViewSupport, viewSupport, restrictedViews, moduleViews, coreViewNames) where

import ConstructionAst
import ConstructionLocals (Locals, ordinaryCoreBindings)
import ConstructionRoles (QuickCheck, isQuickCheckBinding, supportCoreNames)
import ConstructionRuntime
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.List (zip4)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromMaybe, mapMaybe)
import qualified Data.Set as Set
import GHC hiding (exprType)
import GHC.Builtin.Types (liftedTypeKind)
import qualified GHC.Core as C
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.Coercion (coercionLKind, coercionRKind)
import GHC.Core.DataCon (dataConOrigArgTys, dataConUnivTyVars, dataConFullSig)
import GHC.Core.FVs (exprFreeVars)
import qualified GHC.Core.Subst as S
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.TyCon (isAlgTyCon, isVisibleTyConBinder, tyConBinders)
import qualified GHC.Core.Type as T
import GHC.Core.Utils (exprType)
import qualified GHC.Data.Strict as Strict
import GHC.HsToCore.Expr (dsLExpr)
import GHC.HsToCore.Monad (initDs)
import GHC.Tc.Types (tcg_tcs)
import GHC.Types.Id (isDataConId_maybe)
import GHC.Types.Name (nameModule_maybe)
import GHC.Types.SrcLoc (BufSpan, getBufSpan)
import GHC.Types.Var (isId, isTyVar)
import GHC.Types.Var.Env (mkInScopeSet)
import GHC.Types.Var.Set (isEmptyVarSet)

data ViewRuntime = ViewRuntime
    { thunkCon :: DataCon, nilCon :: DataCon, consCon :: DataCon
    , nothingCon :: DataCon, justCon :: DataCon, pairCon :: DataCon
    , voidType :: TyCon, identityName :: Name, absurdName :: Name
    }
data Slot = Slot
    { slotId :: Id, slotType :: TyCon, slotVariables :: [TyVar], slotTemplate :: [Type], slotIndex :: Int
    , slotMapped :: TyVar, slotConvert :: Id, slotInput :: Id, slotBody :: C.CoreExpr
    }
data Policy = Policy { views :: ViewRuntime, maps :: [Id], lists :: [Id], slots :: [Slot] }

data ViewSupport = ViewSupport
    { restrictedViews :: [Name]
    , viewSites :: Map.Map Module ([(SrcSpan, Name)], Set.Set BufSpan)
    }

-- A buffer offset identifies a token only within its own input module.
moduleViews :: ViewSupport -> TypecheckedModule -> ([(SrcSpan, Name)], SrcSpan -> Bool)
moduleViews support checked = (definitions, isCall)
  where
    owner = ms_mod (pm_mod_summary (tm_parsed_module checked))
    (definitions, calls) = Map.findWithDefault ([], Set.empty) owner (viewSites support)
    isCall span' = case getBufSpan span' of
        Strict.Just bytes -> Set.member bytes calls
        Strict.Nothing -> False

coreViewNames :: ViewSupport -> TypecheckedModule -> [Name]
coreViewNames support checked = supportCoreNames (fst (moduleViews support checked)) checked

viewSupport :: Runtime -> QuickCheck -> Locals -> [Module] -> [(TypecheckedModule, [C.CoreBind])] -> Ghc ViewSupport
viewSupport runtime' qc locals homes checkedPrograms = do
    rt <- resolveViews runtime'
    let (checked, programs) = unzip checkedPrograms
        definitions = concatMap allBindings programs
        source = [binding | m <- checked, Just (group, _, _, _, _) <- [tm_renamed_source m],
            binding <- collectAst group :: [HsBind GhcRn]]
        exactSource valid name = any (\binding -> bindingName binding == Just name && valid binding) source
        mapIds = [v | (v, body) <- definitions, exactMap rt v body, exactSource (sourceMap rt) (getName v)]
        first = Policy rt mapIds [] []
        listIds = [v | (v, body) <- definitions, exactList first v body, exactSource (sourceList first (getName v)) (getName v)]
        homeTypes = [tc | m <- checked, tc <- tcg_tcs (fst (tm_internals_ m)),
            isAlgTyCon tc, not (isClassTyCon tc), not (isNewTyCon tc)]
        candidates = mapMaybe (uncurry (slotScheme homeTypes)) definitions
        prune remaining =
            let current = first { lists = listIds, slots = remaining }
                valid = [candidate | candidate <- remaining, exactSlot current candidate,
                    exactSource (sourceSlot current candidate) (getName (slotId candidate))]
            in if length valid == length remaining then valid else prune valid
        policy = first { lists = listIds, slots = prune candidates }
        helpers = helperNames policy
    sites <- forM checked $ \m -> case tm_renamed_source m of
        Nothing -> fail "construction views: renamed source unavailable"
        Just (group, _, _, _, _) -> do
            let owner = ms_mod (pm_mod_summary (tm_parsed_module m))
                support = [(getLocA n, unLoc n) | FunBind _ n _ <- collectAst group :: [HsBind GhcRn], unLoc n `elem` helpers]
                callsites = [e | e <- collectAst group :: [LHsExpr GhcRn], sourceRoot policy homes e]
                typed = collectAst (tm_typechecked_source m) :: [LHsExpr GhcTc]
            session <- getSession
            (_, result) <- liftIO $ initDs session (fst (tm_internals_ m)) $
                mapM (\candidate -> mapM dsLExpr [e | e <- typed, getLocA e == getLocA candidate]) callsites
            let calls = Set.fromList [bytes | (candidate, expressions) <- zip callsites (fromMaybe [] result),
                    Strict.Just bytes <- [getBufSpan (getLocA candidate)],
                    any (validRoot policy . typeLambdaBody . administrative) expressions]
            pure (owner, (support, calls))
    let problems = concat [walkBinding qc policy (C.NonRec v rhs) | program <- programs,
            (v, rhs) <- ordinaryCoreBindings locals qc (C.flattenBinds program)]
    unlessNull problems
    pure (ViewSupport helpers (Map.fromList sites))
  where
    unlessNull [] = pure ()
    unlessNull problems = fail (unlines problems)

resolveViews :: Runtime -> Ghc ViewRuntime
resolveViews runtime' = do
    thunk <- constructor (roleNames runtime' View)
    nil <- named "Memorist.Data.List" "NilT"
    cons <- named "Memorist.Data.List" "ConsT"
    nothing <- named "Memorist.Data.Interface.Data" "NothingT"
    just <- named "Memorist.Data.Interface.Data" "JustT"
    pair <- named "Memorist.Data.Interface.Data" "PairT"
    voidModule <- findModule (mkModuleName "Data.Void") Nothing
    void <- resolveType voidModule "Void"
    ident <- standard "Prelude.id"
    absurd <- standard "Data.Void.absurd"
    pure (ViewRuntime thunk nil cons nothing just pair void ident absurd)
  where
    standard label = case [name | (key, name) <- standardNames runtime', key == label] of
        [name] -> pure name
        _ -> fail ("construction views: missing exact standard identity: " ++ label)
    named label occurrence = do
        constructor [name | name <- roleNames runtime' Representation, describeName name == label ++ "." ++ occurrence]
    constructor [name] = do
        thing <- lookupName name
        case thing of
            Just (AConLike (RealDataCon actual)) -> pure actual
            _ -> fail "construction views: expected a real constructor"
    constructor _ = fail "construction views: ambiguous constructor"

isVar :: Id -> C.CoreExpr -> Bool
isVar x (C.Var y) = x == y
isVar _ _ = False

conCall :: DataCon -> C.CoreExpr -> Maybe [C.CoreExpr]
conCall dc expression = case C.collectArgs expression of
    (C.Var v, args) | isDataConId_maybe v == Just dc -> Just args
    _ -> Nothing

unary :: TyCon -> Type -> Maybe Type
unary tc ty = case T.splitTyConApp_maybe ty of
    Just (actual, [a]) | actual == tc -> Just a
    _ -> Nothing

mapScheme :: TyCon -> Id -> C.CoreExpr -> Maybe (TyVar, TyVar, Id, Id, C.CoreExpr)
mapScheme tc v expression = case C.collectBinders expression of
    ([a, b, convert, input], body)
        | isTyVar a, isTyVar b, a /= b, isId convert, isId input
        , eqType (T.tyVarKind a) liftedTypeKind, eqType (T.tyVarKind b) liftedTypeKind
        , let at = T.mkTyVarTy a; bt = T.mkTyVarTy b
        , eqType (idType convert) (T.mkVisFunTyMany at bt)
        , eqType (idType input) (T.mkTyConApp tc [at])
        , eqType (exprType body) (T.mkTyConApp tc [bt])
        , eqType (idType v) (exprType expression) -> Just (a, b, convert, input, body)
    _ -> Nothing

exactMap :: ViewRuntime -> Id -> C.CoreExpr -> Bool
exactMap rt v expression = case mapScheme (dataConTyCon (thunkCon rt)) v expression of
    Just (_, b, convert, input, C.Case scrutinee _ result [C.Alt (C.DataAlt dc) [name, payload, metadata] out])
        | dc == thunkCon rt, isVar input scrutinee
        , eqType result (T.mkTyConApp (dataConTyCon dc) [T.mkTyVarTy b])
        , Just [C.Type ty, identity, C.App f x, annotation] <- conCall dc out
        , eqType ty (T.mkTyVarTy b), isVar name identity, isVar convert f
        , isVar payload x, isVar metadata annotation -> True
    _ -> False

viewCall :: [Id] -> C.CoreExpr -> Maybe (Type, Type, C.CoreExpr, C.CoreExpr)
viewCall ids expression = case C.collectArgs expression of
    (C.Var v, [C.Type a, C.Type b, f, input]) | v `elem` ids -> Just (a, b, f, input)
    _ -> Nothing

alt :: DataCon -> [C.CoreAlt] -> Maybe ([Id], C.CoreExpr)
alt dc alternatives = case [(vs, e) | C.Alt (C.DataAlt actual) vs e <- alternatives, actual == dc] of
    [result] -> Just result
    _ -> Nothing

exactList :: Policy -> Id -> C.CoreExpr -> Bool
exactList policy self expression = case mapScheme (dataConTyCon (nilCon rt)) self expression of
    Just (a, b, convert, input, C.Case scrutinee _ _ alternatives)
        | isVar input scrutinee, length alternatives == 2
        , Just ([], empty) <- alt (nilCon rt) alternatives
        , Just [C.Type bt] <- conCall (nilCon rt) empty, eqType bt (T.mkTyVarTy b)
        , Just ([first, rest], out) <- alt (consCon rt) alternatives
        , Just [C.Type bt', headOut, tailOut] <- conCall (consCon rt) out, eqType bt' bt
        , Just (ha, hb, hf, hx) <- viewCall (maps policy) headOut
        , eqType ha (T.mkTyVarTy a), eqType hb bt, isVar convert hf, isVar first hx
        , Just (ta, tb, tf, tx) <- viewCall (maps policy) tailOut
        , eqType ta (idType input), eqType tb (exprType out), isVar rest tx
        , (C.Var recursive, [C.Type ra, C.Type rb, f]) <- C.collectArgs tf
        , recursive == self, eqType ha ra, eqType hb rb, isVar convert f -> True
    _ -> False
  where rt = views policy

converter :: Policy -> Type -> Type -> C.CoreExpr -> Bool
converter policy = converterWith policy Nothing

converterWith :: Policy -> Maybe (Id, Type, Type) -> Type -> Type -> C.CoreExpr -> Bool
converterWith policy context a b expression
    | not (eqType (exprType expression) (T.mkVisFunTyMany a b)) = False
    | Just (leaf, from, to) <- context, isVar leaf expression, eqType a from, eqType b to = True
    | Just (slot, arguments, mapped, [f]) <- slotApplication policy 1 expression,
      let input = T.mkTyConApp (slotType slot) arguments,
      let output = T.mkTyConApp (slotType slot) (replace (slotIndex slot) mapped arguments),
      Just from <- index (slotIndex slot) arguments,
      eqType a input, eqType b output = converterWith policy context from mapped f
    | otherwise = case C.collectArgs expression of
        (C.Var v, [C.Type ty]) | getName v == identityName rt -> eqType a b && eqType a ty
        (C.Var v, [C.Type ty]) | getName v == absurdName rt -> eqType a (T.mkTyConTy (voidType rt)) && eqType b ty
        (C.Var v, [C.Type x, C.Type y, f]) | v `elem` lists policy ->
            maybe False (`eqType` x) (unary (dataConTyCon (nilCon rt)) a) &&
            maybe False (`eqType` y) (unary (dataConTyCon (nilCon rt)) b) && converterWith policy context x y f
        _ -> case expression of
            C.Lam x body | isId x, eqType (idType x) a ->
                identityBody x body || maybeBody x body || pairBody x body || slotChain policy context x a b body
            _ -> False
  where
    rt = views policy
    identityBody x body = case C.collectArgs body of
        (C.Var y, args) | x == y, all C.isTypeArg args ->
            let actual = [ty | C.Type ty <- args]; (bound, _) = T.splitForAllTyVars a
            in (null actual && eqType a b) ||
                (not (null bound) && length bound == length actual && eqType (T.piResultTys a actual) b)
        _ -> False
    maybeBody x (C.Case scrutinee _ result alternatives)
        | isVar x scrutinee, eqType result b, length alternatives == 2
        , Just ai <- unary (dataConTyCon (nothingCon rt)) a, Just bi <- unary (dataConTyCon (nothingCon rt)) b
        , Just ([], empty) <- alt (nothingCon rt) alternatives
        , Just [C.Type bt] <- conCall (nothingCon rt) empty, eqType bt bi
        , Just ([field], out) <- alt (justCon rt) alternatives
        , Just [C.Type bt', mapped] <- conCall (justCon rt) out, eqType bt' bi = fieldView ai bi field mapped
    maybeBody _ _ = False
    pairBody x (C.Case scrutinee _ result alternatives)
        | isVar x scrutinee, eqType result b, length alternatives == 1
        , Just (tc, [al, ar]) <- T.splitTyConApp_maybe a, tc == dataConTyCon (pairCon rt)
        , Just (tc', [bl, br]) <- T.splitTyConApp_maybe b, tc' == tc
        , Just ([left, right], out) <- alt (pairCon rt) alternatives
        , Just [C.Type bl', C.Type br', l, r] <- conCall (pairCon rt) out
        , eqType bl bl', eqType br br' = fieldView al bl left l && fieldView ar br right r
    pairBody _ _ = False
    fieldView ai bi field mapped
        | eqType ai bi, isVar field mapped = True
        | otherwise = case viewCall (maps policy) mapped of
            Just (ai', bi', f, input) -> eqType ai ai' && eqType bi bi' && isVar field input && converterWith policy context ai bi f
            _ -> False

validRoot :: Policy -> C.CoreExpr -> Bool
validRoot policy expression = case viewCall (maps policy ++ lists policy) expression of
    Just (a, b, f, input) -> converter policy a b f && neutral input
    _ -> case slotApplication policy 2 expression of
        Just (slot, arguments, mapped, [f, input]) | Just from <- index (slotIndex slot) arguments ->
            converter policy from mapped f && neutral input
        _ -> False
  where
    neutral input = case C.collectArgs input of
        (C.Var v, args) -> getName v `notElem` helperNames policy &&
            isDataConId_maybe v /= Just (thunkCon (views policy)) && all C.isTypeArg args
        _ -> False

typeLambdaBody :: C.CoreExpr -> C.CoreExpr
typeLambdaBody (C.Lam v body) | isTyVar v = typeLambdaBody body
typeLambdaBody body = body

-- Only case aliases and constructor-wrapper beta redexes are administrative.
-- Never unfold a helper, inspect IdInfo, or interpret an arbitrary converter.
administrative :: C.CoreExpr -> C.CoreExpr
administrative = go
  where
    substitute v rhs body =
        let empty = S.mkEmptySubst (mkInScopeSet (exprFreeVars (C.App (C.Lam v body) rhs)))
            substitution = case rhs of C.Type ty | isTyVar v -> S.extendTvSubst empty v ty; _ -> S.extendIdSubst empty v rhs
        in S.substExpr substitution body
    constructorBody body = case C.collectArgs (snd (C.collectBinders body)) of
        (C.Var v, _) -> isJust (isDataConId_maybe v)
        _ -> False
    go (C.App f x) = let f' = go f; x' = go x in case f' of
        C.Lam v body | constructorBody body -> go (substitute v x' body)
        _ -> C.App f' x'
    go (C.Let (C.NonRec v (C.Var alias)) body) = go (substitute v (C.Var alias) body)
    go (C.Lam v body) = C.Lam v (go body)
    go (C.Case scrutinee v ty alternatives) = C.Case (go scrutinee) v ty [C.Alt dc vs (go rhs) | C.Alt dc vs rhs <- alternatives]
    go body = body

walkBinding :: QuickCheck -> Policy -> C.CoreBind -> [String]
walkBinding qc policy binding = concat [walk (nameSrcSpan (getName v)) expression
    | (v, expression) <- C.flattenBinds [binding], not (isQuickCheckBinding qc v), getName v `notElem` helperNames policy]
  where
    walk location expression = case C.collectArgs expression of
        (C.Var v, _) | getName v `elem` helperNames policy ->
            [pretty location ++ ": unchecked structural view use/escape: " ++ pretty expression | not (validRoot policy expression)]
        _ -> case expression of
            C.App f x -> recur f ++ recur x
            C.Lam _ body -> recur body
            C.Let b body -> walkBinding qc policy b ++ recur body
            C.Case scrutinee _ _ alternatives -> recur scrutinee ++ concat [recur rhs | C.Alt _ _ rhs <- alternatives]
            C.Cast body _ -> recur body
            C.Tick _ body -> recur body
            _ -> []
      where recur = walk location

sourceMap :: ViewRuntime -> HsBind GhcRn -> Bool
sourceMap rt binding = case equations binding of
    Just [([f, pat], body)] | Just convert <- plainPattern f,
            Just pats <- constructorPattern (getName (thunkCon rt)) pat,
            Just [name, payload, metadata] <- mapM plainPattern pats,
            Just [identity, mapped, annotation] <- call [getName (thunkCon rt)] body,
            variable identity == Just name, variable annotation == Just metadata,
            Just [input] <- call [convert] mapped, variable input == Just payload -> True
    _ -> False

sourceAlt :: Name -> MatchGroup GhcRn (LHsExpr GhcRn) -> Maybe ([Name], LHsExpr GhcRn)
sourceAlt constructor matches = case
        [(fields, body) | L _ match <- unLoc (mg_alts matches), [pat] <- [m_pats match],
            Just pats <- [constructorPattern constructor pat], Just fields <- [mapM plainPattern pats], Just body <- [oneRhs match]] of
    [result] -> Just result
    _ -> Nothing

sourceList :: Policy -> Name -> HsBind GhcRn -> Bool
sourceList policy self binding = case equations binding of
    Just [([f, x], expression)] | Just convert <- plainPattern f, Just input <- plainPattern x,
            (L _ (HsCase _ scrutinee alternatives), []) <- spine expression, variable scrutinee == Just input,
            length (unLoc (mg_alts alternatives)) == 2,
            Just ([], empty) <- sourceAlt (getName (nilCon rt)) alternatives, variable empty == Just (getName (nilCon rt)),
            Just ([first, rest], out) <- sourceAlt (getName (consCon rt)) alternatives,
            Just [headOut, tailOut] <- call [getName (consCon rt)] out,
            Just [hf, hx] <- call (map getName (maps policy)) headOut, variable hf == Just convert, variable hx == Just first,
            Just [tf, tx] <- call (map getName (maps policy)) tailOut, variable tx == Just rest,
            Just [cf] <- call [self] tf, variable cf == Just convert -> True
    _ -> False
  where rt = views policy

sourceConverter :: Policy -> LHsExpr GhcRn -> Bool
sourceConverter policy = sourceConverterWith policy Nothing

sourceConverterWith :: Policy -> Maybe Name -> LHsExpr GhcRn -> Bool
sourceConverterWith policy context expression
    | Just name <- context, variable expression == Just name = True
    | variable expression `elem` map Just [identityName rt, absurdName rt] = True
    | Just [f] <- call (map getName (lists policy) ++ map (getName . slotId) (slots policy)) expression = sourceConverterWith policy context f
    | (L _ (HsLam _ LamSingle matches), []) <- spine expression,
      [L _ match] <- unLoc (mg_alts matches), [pat] <- m_pats match, Just input <- plainPattern pat,
      Just body <- oneRhs match = variable body == Just input || maybeBody input body || pairBody input body || sourceSlotChain policy context input body
    | otherwise = False
  where
    rt = views policy
    fieldView field out
        | variable out == Just field = True
        | otherwise = case call (map getName (maps policy)) out of
            Just [f, input] -> variable input == Just field && sourceConverterWith policy context f
            _ -> False
    maybeBody input body | (L _ (HsCase _ scrutinee alternatives), []) <- spine body, variable scrutinee == Just input,
            length (unLoc (mg_alts alternatives)) == 2,
            Just ([], empty) <- sourceAlt (getName (nothingCon rt)) alternatives, variable empty == Just (getName (nothingCon rt)),
            Just ([field], out) <- sourceAlt (getName (justCon rt)) alternatives,
            Just [mapped] <- call [getName (justCon rt)] out = fieldView field mapped
    maybeBody _ _ = False
    pairBody input body | (L _ (HsCase _ scrutinee alternatives), []) <- spine body, variable scrutinee == Just input,
            length (unLoc (mg_alts alternatives)) == 1,
            Just ([left, right], out) <- sourceAlt (getName (pairCon rt)) alternatives,
            Just [l, r] <- call [getName (pairCon rt)] out = fieldView left l && fieldView right r
    pairBody _ _ = False

sourceRoot :: Policy -> [Module] -> LHsExpr GhcRn -> Bool
sourceRoot policy homes expression = case call (helperNames policy) expression of
    Just [f, input] | sourceConverter policy f, Just name <- variable input ->
        name `notElem` helperNames policy &&
            maybe True (`elem` homes) (nameModule_maybe name)
    _ -> False

helperNames :: Policy -> [Name]
helperNames policy = map getName (maps policy ++ lists policy) ++ map (getName . slotId) (slots policy)

-- Only a visible Type slot changes. Every other actual parameter and kind is
-- fixed. The type constructor must come from the checked home declarations.
slotScheme :: [TyCon] -> Id -> C.CoreExpr -> Maybe Slot
slotScheme homeTypes identifier expression = case C.collectBinders expression of
    (binders, body) -> case span isTyVar binders of
        (variables, [convert, input]) | isId convert, isId input,
                mapped : originalReversed <- reverse variables,
                let original = reverse originalReversed,
                Just (tc, arguments) <- T.splitTyConApp_maybe (idType input), tc `elem` homeTypes,
                length arguments == length (tyConBinders tc),
                mapMaybe T.getTyVar_maybe arguments == original,
                and (zipWith originalArgument (tyConBinders tc) arguments),
                Just (outTc, results) <- T.splitTyConApp_maybe (exprType body), outTc == tc, length results == length arguments,
                [(position, from, to)] <- [(i, a, b) | (i, a, b) <- zip3 [0..] arguments results, not (eqType a b)],
                eqType to (T.mkTyVarTy mapped), eqType (T.tyVarKind mapped) liftedTypeKind,
                Just selected <- T.getTyVar_maybe from, eqType (T.tyVarKind selected) liftedTypeKind,
                Just selectedBinder <- index position (tyConBinders tc), isVisibleTyConBinder selectedBinder,
                eqType (idType convert) (T.mkVisFunTyMany from to),
                eqType (idType identifier) (exprType expression),
                all simpleConstructor (tyConDataCons tc) ->
            Just (Slot identifier tc original arguments position mapped convert input body)
        _ -> Nothing
  where
    simpleConstructor dc = case dataConFullSig dc of
        (_, [], [], [], _, _) -> True
        _ -> False
    originalArgument binder ty = isJust (T.getTyVar_maybe ty) ||
        (not (isVisibleTyConBinder binder) && isEmptyVarSet (tyCoVarsOfType ty))

exactSlot :: Policy -> Slot -> Bool
exactSlot policy slot = case slotBody slot of
    -- GHC erases a complete constructor-preserving map of a phantom slot to
    -- this identity cast. Its permission comes from the checked declarations
    -- and literal field provenance below, never from a cast/signature alone.
    C.Cast input coercion | isVar (slotInput slot) input,
            eqType (coercionLKind coercion) (idType (slotInput slot)), eqType (coercionRKind coercion) output ->
        all (\dc -> let (before, after) = slotFields slot dc in and (zipWith eqType before after)) constructors
    C.Case scrutinee _ result alternatives | isVar (slotInput slot) scrutinee,
            eqType result output, length alternatives == length constructors -> all valid constructors
      where
        valid dc = case alt dc alternatives of
            Just (fields, out) | Just arguments <- conCall dc out,
                    let (types, values) = span C.isTypeArg arguments,
                    and [eqType actual expected | (C.Type actual, expected) <- zip types outputArguments],
                    length types == length outputArguments,
                    length fields == length inputFields, length values == length fields,
                    and (zipWith (eqType . idType) fields inputFields) -> and
                [fieldView field before after value | (field, before, after, value) <- zip4 fields inputFields outputFields values]
              where
                (inputFields, outputFields) = slotFields slot dc
            _ -> False
    _ -> False
  where
    constructors = tyConDataCons (slotType slot)
    inputArguments = slotTemplate slot
    outputArguments = replace (slotIndex slot) (T.mkTyVarTy (slotMapped slot)) inputArguments
    output = T.mkTyConApp (slotType slot) outputArguments
    context = (slotConvert slot,, T.mkTyVarTy (slotMapped slot)) <$> index (slotIndex slot) inputArguments
    fieldView field before after value
        | eqType before after = isVar field value
        | Just a <- unary (dataConTyCon (thunkCon (views policy))) before,
          Just b <- unary (dataConTyCon (thunkCon (views policy))) after,
          Just (a', b', f, input) <- viewCall (maps policy) value =
            eqType a a' && eqType b b' && isVar field input && converterWith policy context a b f
        | otherwise = False

-- A use instantiates an already verified finite contract. Nonregular recursive
-- types never cause recursive expansion of a datatype definition here.
slotApplication :: Policy -> Int -> C.CoreExpr -> Maybe (Slot, [Type], Type, [C.CoreExpr])
slotApplication policy arity expression = case C.collectArgs expression of
    (C.Var identifier, arguments) | [slot] <- [candidate | candidate <- slots policy, slotId candidate == identifier],
            let (types, terms) = span C.isTypeArg arguments, length terms == arity,
            length types == length (slotVariables slot) + 1,
            C.Type mapped : originalReversed <- reverse types ->
        let actual = reverse [ty | C.Type ty <- originalReversed]
        in Just (slot, map (T.substTyWith (slotVariables slot) actual) (slotTemplate slot), mapped, terms)
    _ -> Nothing

slotChain :: Policy -> Maybe (Id, Type, Type) -> Id -> Type -> Type -> C.CoreExpr -> Bool
slotChain policy context input from to expression
    | isVar input expression = eqType from to
    | Just (slot, arguments, mapped, [f, prior]) <- slotApplication policy 2 expression,
      Just before <- index (slotIndex slot) arguments,
      let intermediate = T.mkTyConApp (slotType slot) arguments,
      eqType to (T.mkTyConApp (slotType slot) (replace (slotIndex slot) mapped arguments)) =
        converterWith policy context before mapped f && slotChain policy context input from intermediate prior
    | otherwise = False

sourceSlot :: Policy -> Slot -> HsBind GhcRn -> Bool
sourceSlot policy slot binding = case equations binding of
    Just [([convertPat, inputPat], expression)] | Just convert <- plainPattern convertPat, Just input <- plainPattern inputPat,
            (L _ (HsCase _ scrutinee alternatives), []) <- spine expression, variable scrutinee == Just input,
            length (unLoc (mg_alts alternatives)) == length constructors -> all (branch convert alternatives) constructors
    _ -> False
  where
    constructors = tyConDataCons (slotType slot)
    branch convert alternatives dc = case sourceAlt (getName dc) alternatives of
        Just (fields, expression) | Just arguments <- call [getName dc] expression, length fields == length arguments,
                let (before, after) = slotFields slot dc, length fields == length before -> and
            [if eqType a b then variable value == Just field else case call (map getName (maps policy)) value of
                Just [f, input] -> variable input == Just field && sourceConverterWith policy (Just convert) f
                _ -> False | (field, value, a, b) <- zip4 fields arguments before after]
        _ -> False

sourceSlotChain :: Policy -> Maybe Name -> Name -> LHsExpr GhcRn -> Bool
sourceSlotChain policy context input expression
    | variable expression == Just input = True
    | Just [f, prior] <- call (map (getName . slotId) (slots policy)) expression =
        sourceConverterWith policy context f && sourceSlotChain policy context input prior
    | otherwise = False

replace :: Int -> a -> [a] -> [a]
replace position value values = [if i == position then value else original | (i, original) <- zip [0..] values]

index :: Int -> [a] -> Maybe a
index position values = case drop position values of value : _ -> Just value; _ -> Nothing

slotFields :: Slot -> DataCon -> ([Type], [Type])
slotFields slot dc = (instantiate before, instantiate after)
  where
    before = slotTemplate slot
    after = replace (slotIndex slot) (T.mkTyVarTy (slotMapped slot)) before
    instantiate arguments = map (T.substTyWith (dataConUnivTyVars dc) arguments . scaledThing) (dataConOrigArgTys dc)
