-- | Narrow, declaration-specific observation roles. These are not exemptions
-- for pure functions: an unrecognized body is checked as construction code.
module ConstructionRoles (supportBindings, supportCoreNames, constraintWrappers, unwrapConstraint, stripConstraints) where

import ConstructionAst
import ConstructionRuntime
import Control.Monad (forM)
import Data.Maybe (mapMaybe)
import GHC
import GHC.Builtin.Types (constraintKind, liftedTypeKind)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type (mkTyVarTy, splitForAllTyVars, splitFunTys, splitTyConApp_maybe, tyVarKind)

-- The helper is an identity on its constrained continuation. Check both its
-- complete source body and its actual rank-2 Proxy contract, never its name.
constraintWrappers :: [TypecheckedModule] -> Ghc [(Name, Name)]
constraintWrappers checked = do
    owner <- findModule (mkModuleName "Data.Proxy") Nothing
    proxy <- resolveType owner "Proxy"
    constructor <- resolveExport owner "Proxy"
    fmap concat $ forM [binding | m <- checked, Just (group, _, _, _, _) <- [tm_renamed_source m],
            binding <- collectAst group :: [HsBind GhcRn]] $ \binding -> case binding of
        FunBind _ name _ | Just [([L _ (WildPat _), pat], body)] <- equations binding,
                Just input <- plainPattern pat, variable body == Just input -> do
            thing <- lookupName (unLoc name)
            pure [(unLoc name, constructor) | Just (AnId identifier) <- [thing], wrapperType proxy (idType identifier)]
        _ -> pure []
  where
    wrapperType proxy ty = case splitForAllTyVars ty of
        ([c, r], body) | eqType (tyVarKind c) constraintKind, eqType (tyVarKind r) liftedTypeKind,
                let predicate = mkTyVarTy c; result = mkTyVarTy r -> case splitFunTys body of
            ([dictionary, probe, continuation], output)
                | eqType (scaledThing dictionary) predicate, eqType output result,
                  Just (actual, [kind, constraint]) <- splitTyConApp_maybe (scaledThing probe),
                  actual == proxy, eqType kind constraintKind, eqType constraint predicate,
                  ([given], value) <- splitFunTys (scaledThing continuation) ->
                    eqType (scaledThing given) predicate && eqType value result
            _ -> False
        _ -> False

unwrapConstraint :: [(Name, Name)] -> LHsExpr GhcRn -> Maybe (LHsExpr GhcRn)
unwrapConstraint wrappers expression = case
        [body | (helper, proxy) <- wrappers, Just [probe, body] <- [call [helper] expression], variable probe == Just proxy] of
    [body] -> Just body
    _ -> Nothing

stripConstraints :: [(Name, Name)] -> LHsExpr GhcRn -> LHsExpr GhcRn
stripConstraints wrappers expression = maybe expression (stripConstraints wrappers) (unwrapConstraint wrappers expression)

supportBindings :: Runtime -> [Name] -> [(Name, Name)] -> HsGroup GhcRn -> [(SrcSpan, Name)]
supportBindings rt constructors wrappers group =
    [(getLocA name, unLoc name) | binding@(FunBind _ name _) <- collectAst group :: [HsBind GhcRn],
        case roleOf rt (unLoc name) of
            Just AbstractMethod -> exactAbstract binding
            Just FullMethod -> exactFull binding
            Just UsageMethod -> exactUsage binding
            _ -> False]
  where
    names = roleNames rt
    exactAbstract binding = allEquations wrappers binding $ \pats body -> case pats of
        [pat] | Just input <- plainPattern pat, variable body == Just input -> True
        [pat] | Just fields <- fieldsOf pat, Just (_, arguments) <- dataCall body, length fields == length arguments ->
            and (zipWith abstractField fields arguments)
        _ -> False
    abstractField field expression = case call (names AbstractMethod) expression of
        Just [projected] -> case call (names Payload) projected of
            Just [input] -> variable input == Just field
            _ -> False
        _ -> False
    exactFull binding = allEquations wrappers binding $ \pats body -> case pats of
        [pat] | Just input <- plainPattern pat, variable body == Just input -> True
        [pat] | Just fields <- fieldsOf pat, Just (_, arguments) <- dataCall body, length fields == length arguments ->
            and (zipWith (\field expression -> case call (names FullMethod) expression of
                Just [input] -> variable input == Just field
                _ -> False) fields arguments)
        _ -> False
    exactUsage binding = allEquations wrappers binding $ \pats body -> case pats of
        [pat, demand] | Just fields <- fieldsOf pat, Just demands <- fieldsOf demand, length fields == length demands ->
            accounting (zip fields demands) body
        [L _ (WildPat _), L _ (WildPat _)] -> variable body `elem` map Just (names EmptyAccounting)
        _ -> False
    accounting [] expression = variable expression `elem` map Just (names EmptyAccounting)
    accounting ((field, demand) : rest) expression = case unLoc (fst (spine expression)) of
        OpApp _ left operator right | variable operator `elem` map Just (names UnionAccounting) ->
            case call (names UsageMethod) left of
                Just [input, demanded] -> variable input == Just field && variable demanded == Just demand && accounting rest right
                _ -> False
        _ -> False
    fieldsOf (L _ (ParPat _ pat)) = fieldsOf pat
    fieldsOf (L _ (ConPat _ constructor (PrefixCon [] pats)))
        | unLoc constructor `elem` constructors = mapM plainPattern pats
    fieldsOf _ = Nothing
    dataCall expression = case spine expression of
        (L _ (HsVar _ constructor), arguments) | unLoc constructor `elem` constructors -> Just (unLoc constructor, arguments)
        _ -> Nothing

allEquations :: [(Name, Name)] -> HsBind GhcRn -> ([LPat GhcRn] -> LHsExpr GhcRn -> Bool) -> Bool
allEquations wrappers binding valid = case equations binding of
    Just cases -> not (null cases) && and [valid pats (stripConstraints wrappers body) | (pats, body) <- cases]
    Nothing -> False

-- GHC gives instance methods fresh internal identifiers. Follow only the finite
-- AbsBinds export relation, not name spellings or IdInfo/unfoldings.
supportCoreNames :: [(SrcSpan, Name)] -> TypecheckedModule -> [Name]
supportCoreNames support checked = close initial
  where
    initial = [getName (unLoc name) | FunBind _ name _ <- collectAst (tm_typechecked_source checked) :: [HsBind GhcTc],
        getLocA name `elem` map fst support]
    exports = [(getName poly, getName mono) | ABE poly mono _ _ <- collectAst (tm_typechecked_source checked) :: [ABExport]]
    close names = let more = names ++ mapMaybe (\(poly, mono) -> if mono `elem` names && poly `notElem` names then Just poly else Nothing) exports
                  in if length more == length names then names else close more
