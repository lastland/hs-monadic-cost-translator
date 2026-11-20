-- | Traverse finite compiler syntax, never IdInfo, unfoldings, or wrappers.
module ConstructionAst (collectAst, children, atSourceSpan, reachable, allBindings, nestedBindings, pretty, spine, firstOrderCall, variable, typedVariable, call, bindingName, plainPattern, constructorPattern, oneRhs, equations) where

import Data.Data (Data, cast, gmapQ)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, maybeToList)
import qualified Data.Set as Set
import GHC
import GHC.Builtin.Names (dollarName)
import qualified GHC.Core as C
import GHC.Core.ConLike (ConLike)
import GHC.Tc.Types.Evidence (HsWrapper, TcEvBinds)
import GHC.Types.SrcLoc (getBufSpan, srcSpanToRealSrcSpan)
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

-- Index one module once. Keep every candidate in source order: GHC can
-- retain several typed expressions at one token, with different wrappers.
atSourceSpan :: [(SrcSpan, a)] -> SrcSpan -> [a]
atSourceSpan entries = \at -> [value | (location, value) <- Map.findWithDefault [] (key at) index, location == at]
  where
    key at = (srcSpanToRealSrcSpan at, getBufSpan at)
    index = Map.fromListWith (++) [(key at, [entry]) | entry@(at, _) <- reverse entries]

reachable :: Map.Map Name [Name] -> Set.Set Name -> Set.Set Name
reachable edges initial = go initial (Set.toList initial)
  where
    go seen [] = seen
    go seen (n:rest) = let next = Set.fromList (Map.findWithDefault [] n edges) `Set.difference` seen
                       in go (Set.union seen next) (Set.toList next ++ rest)

-- Visit each binding immediately before the let bindings in its RHS.
allBindings :: C.CoreProgram -> [(Id, C.CoreExpr)]
allBindings = concatMap tree . C.flattenBinds
  where tree pair@(_, rhs) = pair : concatMap tree (nestedBindings rhs)

-- Immediate binding boundaries; leave each binding's RHS to its own walk.
nestedBindings :: C.CoreExpr -> [(Id, C.CoreExpr)]
nestedBindings (C.Let b body) = C.flattenBinds [b] ++ nestedBindings body
nestedBindings (C.App f x) = nestedBindings f ++ nestedBindings x
nestedBindings (C.Lam _ body) = nestedBindings body
nestedBindings (C.Case scrutinee _ _ alts) = nestedBindings scrutinee ++ concatMap nestedBindings (C.rhssOfAlts alts)
nestedBindings (C.Cast body _) = nestedBindings body
nestedBindings (C.Tick _ body) = nestedBindings body
nestedBindings _ = []

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

-- The wired-in ($) supplies an argument to its left operand. A different
-- infix function receives both operands itself.
firstOrderCall :: LHsExpr GhcRn -> (LHsExpr GhcRn, [LHsExpr GhcRn])
firstOrderCall e = case spine e of
    (L _ (OpApp _ f op x), rest) | variable op == Just dollarName -> apply f (x : rest)
                                 | otherwise -> (op, f : x : rest)
    (op, f : x : rest) | variable op == Just dollarName -> apply f (x : rest)
    callSite -> callSite
  where apply f xs = let (h, args) = firstOrderCall f in (h, args ++ xs)

-- A type application or compiler wrapper can carry the instantiation. Keep
-- the source variable's site; never borrow the type of a surrounding term call.
typedVariable :: LHsExpr GhcTc -> Maybe (SrcSpan, Name)
typedVariable expression = at (getLocA expression) (unLoc expression)
  where
    at location (HsVar _ name) = Just (location, getName (unLoc name))
    at location (HsRecSel _ field) = Just (location, getName (foExt field))
    at _ (HsAppType _ function _) = typedVariable function
    at _ (HsPar _ function) = typedVariable function
    at _ (ExprWithTySig _ function _) = typedVariable function
    at location (XExpr (WrapExpr (HsWrap _ function))) = at location function
    at _ _ = Nothing

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
