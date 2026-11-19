-- | Traverse finite compiler syntax, never IdInfo, unfoldings, or wrappers.
module ConstructionAst (collectAst, children, allBindings, pretty, spine, variable, call, bindingName, plainPattern, constructorPattern, oneRhs, equations) where

import Data.Data (Data, cast, gmapQ)
import Data.Maybe (isJust, maybeToList)
import GHC
import qualified GHC.Core as C
import GHC.Core.ConLike (ConLike)
import GHC.Tc.Types.Evidence (HsWrapper, TcEvBinds)
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)

collectAst :: (Data a, Data b) => b -> [a]
collectAst value = maybeToList (cast value) ++ concat (children collectAst value)

children :: Data a => (forall b. Data b => b -> c) -> a -> [c]
children walk value
    | isJust (cast value :: Maybe Id) = []
    | isJust (cast value :: Maybe Type) = []
    | isJust (cast value :: Maybe ConLike) = []
    | isJust (cast value :: Maybe HsWrapper) = []
    | isJust (cast value :: Maybe TcEvBinds) = []
    | otherwise = gmapQ walk value

-- Visit each binding immediately before the let bindings in its RHS.
allBindings :: C.CoreProgram -> [(Id, C.CoreExpr)]
allBindings bindings = concat [pair : inside rhs | pair@(_, rhs) <- C.flattenBinds bindings]
  where
    inside (C.Let b body) = allBindings [b] ++ inside body
    inside (C.App f x) = inside f ++ inside x
    inside (C.Lam _ body) = inside body
    inside (C.Case scrutinee _ _ alternatives) = inside scrutinee ++ concatMap inside (C.rhssOfAlts alternatives)
    inside (C.Cast body _) = inside body
    inside (C.Tick _ body) = inside body
    inside _ = []

pretty :: Outputable a => a -> String
pretty = showSDocUnsafe . ppr

spine :: LHsExpr GhcRn -> (LHsExpr GhcRn, [LHsExpr GhcRn])
spine expression = case unLoc expression of
    HsApp _ f x -> let (h, xs) = spine f in (h, xs ++ [x])
    HsAppType _ f _ -> spine f
    HsPar _ f -> spine f
    ExprWithTySig _ f _ -> spine f
    XExpr (ExpandedThingRn (OrigExpr original) _) -> case expression of
        L location _ -> spine (L location original)
    _ -> (expression, [])

variable :: LHsExpr GhcRn -> Maybe Name
variable expression = case spine expression of
    (L _ (HsVar _ name), []) -> Just (unLoc name)
    _ -> Nothing

call :: [Name] -> LHsExpr GhcRn -> Maybe [LHsExpr GhcRn]
call names expression = case spine expression of
    (L _ (HsVar _ name), args) | unLoc name `elem` names -> Just args
    _ -> Nothing

-- A pattern that defines several values does not have a single result role.
bindingName :: HsBind GhcRn -> Maybe Name
bindingName (FunBind _ name _) = Just (unLoc name)
bindingName PatBind{pat_lhs = pat} = plainPattern pat
bindingName _ = Nothing

plainPattern :: LPat GhcRn -> Maybe Name
plainPattern (L _ (VarPat _ name)) = Just (unLoc name)
plainPattern (L _ (ParPat _ pat)) = plainPattern pat
plainPattern _ = Nothing

constructorPattern :: Name -> LPat GhcRn -> Maybe [LPat GhcRn]
constructorPattern name (L _ (ConPat _ actual (PrefixCon [] pats))) | unLoc actual == name = Just pats
constructorPattern name (L _ (ParPat _ pat)) = constructorPattern name pat
constructorPattern _ _ = Nothing

oneRhs :: Match GhcRn (LHsExpr GhcRn) -> Maybe (LHsExpr GhcRn)
oneRhs match = case m_grhss match of
    GRHSs _ [L _ (GRHS _ [] expression)] (EmptyLocalBinds _) -> Just expression
    _ -> Nothing

equations :: HsBind GhcRn -> Maybe [([LPat GhcRn], LHsExpr GhcRn)]
equations (FunBind _ _ matches) = mapM equation (unLoc (mg_alts matches))
  where
    equation (L _ match) = (m_pats match,) <$> oneRhs match
equations _ = Nothing
