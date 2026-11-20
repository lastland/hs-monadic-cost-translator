-- | Traverse finite compiler syntax, never IdInfo, unfoldings, or wrappers.
module ConstructionAst
    ( collectAst, children, atSourceSpan, allBindings, nestedBindings, pretty, spine, variable, typedVariable
    , strictConstructorOperands, updateAlternatives, call, bindingName, strictNullaryRhs
    , plainPattern, constructorPattern, oneRhs, equations
    ) where

import Data.Data (Data, cast, gmapQ)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, maybeToList)
import GHC
import qualified GHC.Core as C
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.DataCon (dataConImplBangs, isBanged)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type (splitFunTys)
import qualified GHC.Data.Strict as Strict
import GHC.Hs.Syn.Type (hsExprType, lhsExprType)
import GHC.Tc.Types.Evidence (HsWrapper, TcEvBinds)
import GHC.Tc.Utils.TcType (tcSplitSigmaTy)
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

-- A type application or compiler wrapper can carry the instantiation. Keep
-- the source variable's site; never borrow the type of a surrounding term call.
typedHead :: LHsExpr GhcTc -> (SrcSpan, HsExpr GhcTc)
typedHead expression = at (getLocA expression) (unLoc expression)
  where
    at _ (HsAppType _ function _) = typedHead function
    at _ (HsPar _ function) = typedHead function
    at _ (ExprWithTySig _ function _) = typedHead function
    at location (XExpr (WrapExpr (HsWrap _ function))) = at location function
    at location head' = (location, head')

typedVariable :: LHsExpr GhcTc -> Maybe (SrcSpan, Name)
typedVariable expression = case typedHead expression of
    (location, HsVar _ name) -> Just (location, getName (unLoc name))
    (location, HsRecSel _ field) -> Just (location, getName (foExt field))
    _ -> Nothing

-- Field bangs are in declaration order. Read their operand types from the
-- instantiated wrapper, even for a bare constructor with no written arguments.
strictConstructorOperands :: LHsExpr GhcTc -> Maybe (SrcSpan, Name, [Type])
strictConstructorOperands expression = case typedHead expression of
    (loc, XExpr (ConLikeTc (RealDataCon con) _ _)) -> fields loc con (lhsExprType expression)
    (_, RecordCon{rcon_con = n, rcon_ext = actual}) | RealDataCon con <- unLoc n ->
        fields (getLocA n) con (hsExprType actual)
    _ -> Nothing
  where
    fields loc con ty = case getBufSpan loc of
        Strict.Just _ -> let (_, _, body) = tcSplitSigmaTy ty in Just (loc, getName con,
            [scaledThing arg | (bang, arg) <- zip (dataConImplBangs con) (fst (splitFunTys body)), isBanged bang])
        Strict.Nothing -> Nothing

-- GHC 9.10 expands a record update to let/case. Only the generated case
-- alternatives select constructors; a type-changing update uses its result type.
updateAlternatives :: HsExpr GhcTc -> [(ConLike, Type)]
updateAlternatives (HsLet _ _ body) = updateAlternatives (unLoc body)
updateAlternatives (XExpr (WrapExpr (HsWrap _ expression))) = updateAlternatives expression
updateAlternatives (HsCase RecUpd _ matches) =
    [(con, mg_res_ty (mg_ext matches)) | L _ match <- unLoc (mg_alts matches),
        L _ ConPat{pat_con = L _ con} <- m_pats match]
updateAlternatives _ = []

call :: [Name] -> LHsExpr GhcRn -> Maybe [LHsExpr GhcRn]
call names expression = case spine expression of
    (L _ (HsVar _ name), args) | unLoc name `elem` names -> Just args
    _ -> Nothing

-- A pattern that defines several values does not have a single result role.
bindingName :: HsBind GhcRn -> Maybe Name
bindingName (FunBind _ name _) = Just (unLoc name)
bindingName PatBind{pat_lhs = pat} = plainPattern pat
bindingName _ = Nothing

-- GHC retains `let !y = rhs` as a strict equation with no arguments.
strictNullaryRhs :: MatchGroup (GhcPass p) (LHsExpr (GhcPass p)) -> Bool
strictNullaryRhs matches = case unLoc (mg_alts matches) of
    [L _ Match{m_ctxt = FunRhs{mc_strictness = SrcStrict}, m_pats = []}] -> True
    _ -> False

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
