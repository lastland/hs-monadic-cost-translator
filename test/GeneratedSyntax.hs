-- | Views of the published, parsed Haskell used by the structural checks.
module GeneratedSyntax
    ( Expr, Binding, generatedBindings, collectAst, bareExpression, expressionVariable
    , patternVariable, named, application, arguments, oneRhs, terminalExpression
    ) where

import Control.Monad (guard)
import Data.Data (Data, cast, gmapQ)
import Data.Maybe (maybeToList)
import GHC
    ( ExprLStmt, GhcPs, GRHS(..), GRHSs(..), HsBindLR(..), HsDecl(..), HsExpr(..)
    , HsLocalBindsLR(..), HsModule(..), LHsExpr, LPat, Match(..), MatchGroup(..)
    , Pat(..), StmtLR(..), moduleNameString, unLoc
    )
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName(..), rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated(..))
import GeneratedFunctions (parseHaskellModule)

type Expr = LHsExpr GhcPs
type Binding = Match GhcPs Expr

-- Select only module-level equations, regardless of declaration order.
generatedBindings :: FilePath -> IO (String -> IO Binding)
generatedBindings output = do
    ast <- parseHaskellModule output
    pure $ \target -> case [unLoc alternative | L _ (ValD _ (FunBind _ name matches)) <- hsmodDecls ast,
            occNameString (rdrNameOcc (unLoc name)) == target,
            alternative <- unLoc (mg_alts matches)] of
        [binding] -> pure binding
        bindings -> fail (output ++ ": expected one equation for " ++ target ++
                          ", found " ++ show (length bindings))

collectAst :: (Data a, Data b) => b -> [a]
collectAst tree = maybeToList (cast tree) ++ concat (gmapQ collectAst tree)

bareExpression :: Expr -> HsExpr GhcPs
bareExpression expression = case unLoc expression of
    HsPar _ body -> bareExpression body
    ExprWithTySig _ body _ -> bareExpression body
    HsAppType _ body _ -> bareExpression body
    other -> other

expressionVariable :: Expr -> Maybe RdrName
expressionVariable expression = case bareExpression expression of
    HsVar _ name -> Just (unLoc name)
    _ -> Nothing

patternVariable :: LPat GhcPs -> Maybe RdrName
patternVariable (L _ (VarPat _ name)) = Just (unLoc name)
patternVariable (L _ (ParPat _ pat)) = patternVariable pat
patternVariable (L _ (SigPat _ pat _)) = patternVariable pat
patternVariable _ = Nothing

named :: String -> RdrName -> Bool
named expected (Qual qualifier occurrence) =
    expected == moduleNameString qualifier ++ "." ++ occNameString occurrence ||
        (moduleNameString qualifier == "Prelude" && expected == occNameString occurrence)
named expected name = expected == occNameString (rdrNameOcc name)

-- Expose operands in source order for both infix and saturated prefix spelling.
application :: Expr -> (Expr, [Expr])
application = go []
  where
    go args expression = case bareExpression expression of
        HsApp _ f x -> go (x : args) f
        OpApp _ f op x | maybe False (named "$") (expressionVariable op) -> go (x : args) f
        OpApp _ x op y -> (op, x : y : args)
        _ -> (expression, args)

arguments :: String -> Expr -> Maybe [Expr]
arguments name expression = do
    let (head', args) = application expression
    actual <- expressionVariable head'
    guard (named name actual)
    pure args

oneRhs :: Binding -> Maybe Expr
oneRhs match = case m_grhss match of
    GRHSs _ [L _ (GRHS _ [] body)] EmptyLocalBinds{} -> Just body
    _ -> Nothing

terminalExpression :: ExprLStmt GhcPs -> Maybe Expr
terminalExpression statement = case unLoc statement of
    BodyStmt _ expression _ _ -> Just expression
    LastStmt _ expression _ _ -> Just expression
    _ -> Nothing
