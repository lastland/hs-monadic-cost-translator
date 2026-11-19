module Cost.Translate.Frontend.Match.Fallback (withFallback, freeTerms) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Cost.Translate.Frontend.Syntax (collect)
import Cost.Translate.Output qualified as O
import Data.Data (Data, cast, gmapQ)
import Data.List (unsnoc)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import GHC
import GHC.Types.Name.Occurrence (varName)

withFallback ::
    String -> O.Expr -> (O.Expr -> Either String O.Expr) -> Either String O.Expr
withFallback name fallback build = do
    body <- build (O.v name)
    let key = O.rdr varName name
        expressions = collect body :: [HsExpr GhcPs]
        uses = [() | HsVar _ n <- expressions, unLoc n == key]
        shared = O.letE [O.val name [] fallback] body
    pure $ case uses of
        [] -> body
        [_] -> fromMaybe shared (inlineTail key fallback body)
        _ -> shared

inlineTail :: RdrName -> O.Expr -> O.Expr -> Maybe O.Expr
inlineTail name action = descend
    where
        names = freeTerms action
        clear = all (`Set.notMember` names)
        descend (L _ expr) = case expr of
            HsVar _ n | unLoc n == name -> Just action
            HsPar _ body -> mkHsPar <$> descend body
            HsIf _ cond yes no ->
                ((\yes' -> O.ifE cond yes' no) <$> descend yes)
                    <|> (O.ifE cond yes <$> descend no)
            HsCase _ subject matches -> do
                alts <- traverse simpleAlt (unLoc (mg_alts matches))
                O.caseE subject <$> changeOne branch alts
            HsDo _ (DoExpr Nothing) statements -> do
                (prefix, terminal) <- unsnoc (unLoc statements)
                LastStmt _ lastExpr _ _ <- pure (unLoc terminal)
                guard (clear (collectLStmtsBinders CollNoDictBinders prefix))
                O.doE prefix <$> descend lastExpr
            HsLet _ binds body -> do
                guard (clear (collectLocalBinders CollNoDictBinders binds))
                O.doE [noLocA (LetStmt noAnn binds)] <$> descend body
            _ -> Nothing
        branch (pat, body) = do
            guard (clear (collectPatBinders CollNoDictBinders pat))
            (pat,) <$> descend body

simpleAlt :: LMatch GhcPs O.Expr -> Maybe (O.Pat, O.Expr)
simpleAlt (L _ Match {m_pats = [pat], m_grhss = rhs}) = case rhs of
    GRHSs _ [L _ (GRHS _ [] body)] EmptyLocalBinds {} -> Just (pat, body)
    _ -> Nothing
simpleAlt _ = Nothing

changeOne :: (a -> Maybe a) -> [a] -> Maybe [a]
changeOne _ [] = Nothing
changeOne change (x : xs) =
    ((: xs) <$> change x) <|> ((x :) <$> changeOne change xs)

freeTerms :: (Data a) => a -> Set.Set RdrName
freeTerms tree
    | Just (HsVar _ n) <- expression = Set.singleton (unLoc n)
    | Just (HsLet _ binds body) <- expression =
        without (collectLocalBinders CollNoDictBinders binds) (binds, body)
    | Just (HsDo _ (DoExpr Nothing) stmts) <- expression =
        freeStatements (unLoc stmts)
    | Just match <- cast tree :: Maybe (Match GhcPs O.Expr) =
        without
            (collectPatsBinders CollNoDictBinders (m_pats match))
            (m_grhss match)
    | otherwise = Set.unions (gmapQ freeTerms tree)
    where
        expression = cast tree :: Maybe (HsExpr GhcPs)
        without bound sub = freeTerms sub Set.\\ Set.fromList bound

freeStatements :: [O.Stmt] -> Set.Set RdrName
freeStatements [] = Set.empty
freeStatements (s : ss) = case unLoc s of
    BindStmt _ _ rhs -> freeTerms rhs `Set.union` (rest Set.\\ bound)
    LetStmt _ binds -> (freeTerms binds `Set.union` rest) Set.\\ bound
    _ -> freeTerms s `Set.union` rest
    where
        rest = freeStatements ss
        bound = Set.fromList (collectLStmtBinders CollNoDictBinders s)
