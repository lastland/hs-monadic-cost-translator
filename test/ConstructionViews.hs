-- | A metadata view is not an arbitrary (a -> b). Both its declaration and
-- every retained source call need a closed structural certificate. Exporting
-- a checked helper does not certify future external consumers (policy #7).
module ConstructionViews (viewSupport) where

import ConstructionAst
import ConstructionRuntime
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust, fromMaybe)
import GHC hiding (exprType)
import GHC.Builtin.Types (liftedTypeKind)
import qualified GHC.Core as C
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.FVs (exprFreeVars)
import qualified GHC.Core.Subst as S
import GHC.Core.TyCo.Compare (eqType)
import qualified GHC.Core.Type as T
import GHC.Core.Utils (exprType)
import GHC.HsToCore.Expr (dsLExpr)
import GHC.HsToCore.Monad (initDs)
import GHC.Types.Id (isDataConId_maybe)
import GHC.Types.Name (nameModule_maybe)
import GHC.Types.Var (isId, isTyVar)
import GHC.Types.Var.Env (mkInScopeSet)
import GHC.Unit.Module.ModGuts (mg_binds)

data ViewRuntime = ViewRuntime
    { thunkCon :: DataCon, nilCon :: DataCon, consCon :: DataCon
    , nothingCon :: DataCon, justCon :: DataCon, pairCon :: DataCon
    , voidType :: TyCon, identityName :: Name, absurdName :: Name
    }
data Policy = Policy { views :: ViewRuntime, maps :: [Id], lists :: [Id] }

viewSupport :: Runtime -> [Module] -> [TypecheckedModule] -> Ghc ([(SrcSpan, Name)], [Name], [SrcSpan])
viewSupport runtime' homes checked = do
    rt <- resolveViews runtime'
    programs <- mapM (fmap (mg_binds . coreModule) . desugarModule) checked
    let definitions = concatMap (concatMap allBindings) programs
        source = [binding | m <- checked, Just (group, _, _, _, _) <- [tm_renamed_source m],
            binding <- collectAst group :: [HsBind GhcRn]]
        exactSource valid name = any (\binding -> binderName binding == Just name && valid binding) source
        mapIds = [v | (v, body) <- definitions, exactMap rt v body, exactSource (sourceMap rt) (getName v)]
        first = Policy rt mapIds []
        listIds = [v | (v, body) <- definitions, exactList first v body, exactSource (sourceList first (getName v)) (getName v)]
        policy = first { lists = listIds }
        helpers = map getName (mapIds ++ listIds)
        support = [(getLocA name, unLoc name) | FunBind _ name _ <- source, unLoc name `elem` helpers]
    calls <- fmap concat $ forM checked $ \m -> case tm_renamed_source m of
        Nothing -> fail "construction views: renamed source unavailable"
        Just (group, _, _, _, _) -> do
            let candidates = [e | e <- collectAst group :: [LHsExpr GhcRn], sourceRoot policy homes e]
                typed = collectAst (tm_typechecked_source m) :: [LHsExpr GhcTc]
            session <- getSession
            (_, result) <- liftIO $ initDs session (fst (tm_internals_ m)) $
                mapM (\candidate -> mapM dsLExpr [e | e <- typed, getLocA e == getLocA candidate]) candidates
            pure [getLocA candidate | (candidate, expressions) <- zip candidates (fromMaybe [] result),
                any (validRoot policy . typeLambdaBody . administrative) expressions]
    let problems = concatMap (concatMap (walkBinding (Policy rt mapIds listIds))) programs
    unlessNull problems
    pure (support, helpers, calls)
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
    void <- maybe (fail "construction views: missing pre-resolved Void type") pure (lookup "Void" (observationTypes runtime'))
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

pairs :: C.CoreBind -> [(Id, C.CoreExpr)]
pairs (C.NonRec v e) = [(v, e)]
pairs (C.Rec bs) = bs

allBindings :: C.CoreBind -> [(Id, C.CoreExpr)]
allBindings binding = concat [(v, e) : inside e | (v, e) <- pairs binding]
  where
    inside (C.Let b e) = allBindings b ++ inside e
    inside (C.App f x) = inside f ++ inside x
    inside (C.Lam _ e) = inside e
    inside (C.Case e _ _ as) = inside e ++ concat [inside rhs | C.Alt _ _ rhs <- as]
    inside (C.Cast e _) = inside e
    inside (C.Tick _ e) = inside e
    inside _ = []

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
converter policy a b expression
    | not (eqType (exprType expression) (T.mkVisFunTyMany a b)) = False
    | otherwise = case C.collectArgs expression of
        (C.Var v, [C.Type ty]) | getName v == identityName rt -> eqType a b && eqType a ty
        (C.Var v, [C.Type ty]) | getName v == absurdName rt -> eqType a (T.mkTyConTy (voidType rt)) && eqType b ty
        (C.Var v, [C.Type x, C.Type y, f]) | v `elem` lists policy ->
            maybe False (`eqType` x) (unary (dataConTyCon (nilCon rt)) a) &&
            maybe False (`eqType` y) (unary (dataConTyCon (nilCon rt)) b) && converter policy x y f
        _ -> case expression of
            C.Lam x body | isId x, eqType (idType x) a -> identityBody x body || maybeBody x body || pairBody x body
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
    fieldView ai bi field mapped = case viewCall (maps policy) mapped of
        Just (ai', bi', f, input) -> eqType ai ai' && eqType bi bi' && isVar field input && converter policy ai bi f
        _ -> False

validRoot :: Policy -> C.CoreExpr -> Bool
validRoot policy expression = case viewCall (maps policy ++ lists policy) expression of
    Just (a, b, f, input) -> converter policy a b f && case C.collectArgs input of
        (C.Var v, args) -> v `notElem` maps policy ++ lists policy && isDataConId_maybe v /= Just (thunkCon (views policy)) && all C.isTypeArg args
        _ -> False
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

walkBinding :: Policy -> C.CoreBind -> [String]
walkBinding policy binding = concat [walk expression | (v, expression) <- pairs binding, v `notElem` maps policy ++ lists policy]
  where
    walk expression = case C.collectArgs expression of
        (C.Var v, _) | v `elem` maps policy ++ lists policy ->
            ["unchecked structural view use/escape: " ++ pretty expression | not (validRoot policy expression)]
        _ -> case expression of
            C.App f x -> walk f ++ walk x
            C.Lam _ body -> walk body
            C.Let b body -> walkBinding policy b ++ walk body
            C.Case scrutinee _ _ alternatives -> walk scrutinee ++ concat [walk rhs | C.Alt _ _ rhs <- alternatives]
            C.Cast body _ -> walk body
            C.Tick _ body -> walk body
            _ -> []

binderName :: HsBind GhcRn -> Maybe Name
binderName (FunBind _ name _) = Just (unLoc name)
binderName _ = Nothing

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
sourceConverter policy expression
    | variable expression `elem` map Just [identityName rt, absurdName rt] = True
    | Just [f] <- call (map getName (lists policy)) expression = sourceConverter policy f
    | (L _ (HsLam _ LamSingle matches), []) <- spine expression,
      [L _ match] <- unLoc (mg_alts matches), [pat] <- m_pats match, Just input <- plainPattern pat,
      Just body <- oneRhs match = variable body == Just input || maybeBody input body || pairBody input body
    | otherwise = False
  where
    rt = views policy
    fieldView field out = case call (map getName (maps policy)) out of
        Just [f, input] -> variable input == Just field && sourceConverter policy f
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
sourceRoot policy homes expression = case call (map getName (maps policy ++ lists policy)) expression of
    Just [f, input] | sourceConverter policy f, Just name <- variable input ->
        name `notElem` map getName (maps policy ++ lists policy) &&
            maybe True (`elem` homes) (nameModule_maybe name)
    _ -> False
