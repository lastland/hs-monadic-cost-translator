module PaperScalarChecks (checkPaperScalarOutput) where

import Control.Monad (forM, forM_, unless, zipWithM_)
import Data.Either (lefts)
import Data.List (unsnoc)
import Data.Maybe (isNothing)
import GHC
    ( ExprLStmt, GhcPs, HsConDetails(..), HsDoFlavour(..), HsExpr(..), HsOverLit(..), HsType(..)
    , LPat, Match(..), MatchGroup(..), OverLitVal(..), Pat(..), SrcSpan, StmtLR(..)
    , getLocA, unLoc
    )
import GHC.Types.Name.Reader (RdrName)
import GHC.Types.SourceText (il_value)
import GHC.Types.SrcLoc (GenLocated(..))
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import GeneratedSyntax

-- Names in these shapes describe roles, not spellings in the generated file.
-- Each bind extends the lexical scope only for the subsequent statements.
data Shape = Use String | Call String [Shape] | Number Integer
           | Block [Step] Shape | If Shape Shape Shape | Case Shape [Alternative]
data Step = Run Shape | Bind String Shape
type Alternative = (String, [String], Shape)
type Scope = [(RdrName, String)]

checkPaperScalarOutput :: (String -> IO Binding) -> IO ()
checkPaperScalarOutput binding = do
    results <- forM shapes $ \(name, parameters, shape) -> do
        actual <- binding name
        pure $ case checkEquation parameters shape actual of
            Left reason -> Left (name ++ ": " ++ reason)
            Right () -> Right ()
    guards <- forM ["letGuardM", "variableGuardM"] $ \name -> do
        equation <- binding name
        pure $ case failedGuard equation of
            Left reason -> Left (name ++ ": " ++ reason)
            ok -> ok
    payload <- payloadBranch <$> binding "payloadM"
    unless (null (lefts (payload : guards ++ results))) (fail (unlines (lefts (payload : guards ++ results))))
    counts <- forM ["afterAndM", "afterOrM", "nestedAndM", "bangNextM", "literalM", "inspectM"] $ \name -> do
        equation <- binding name
        pure $ case (m_pats equation, oneRhs equation) of
            ([_, p], Just body) | Just x <- patternVariable p ->
                -- Count just the RHS scalar, regardless of the control-flow
                -- encoding. Skipped-path demand is checked by the cost oracle.
                let sites = [() | HsApp _ op arg <- collectAst body,
                                 maybe False (named "M.force") (expressionVariable op),
                                 expressionVariable arg == Just x]
                 in countError name "one RHS input force" (== 1) sites
            ([_], Just body) ->
                -- Count forces through aliases as well as the original thunk.
                let sites = [() | HsVar _ op <- collectAst body :: [HsExpr GhcPs],
                                 named "M.force" (unLoc op)]
                in if name == "bangNextM"
                   then countError name "at most one force for the bang" (<= 1) sites
                   else countError name "one scrutinee force across all rows" (== 1) sites
            _ -> Left (name ++ ": unexpected equation")
    unless (null (lefts counts)) (fail (unlines (lefts counts)))
  where
    countError name wanted ok sites
        | ok (length sites) = Right ()
        | otherwise = Left (name ++ ": expected " ++ wanted ++ ", got " ++ show (length sites))

shapes :: [(String, [String], Shape)]
shapes =
    [ ("takeM", ["count", "list"], entry "count" $
        If (Call "<" [Number 0, value])
            (Block [Bind "xs" (force "list")] $
                Case (Use "xs")
                    [ ("L.NilT", [], nil)
                    , ("L.ConsT", ["head", "tail"],
                        field "-" "takeM" [Use "tail"] "L.ConsT" [Use "head"])
                    ])
            nil)
    , ("boxNextM", ["input"], box)
    , ("boxPrefixM", ["input"], box)
    , ("workM", ["input"], entry "input" (ret (Call "+" [value, value])))
    , ("constantAndM", ["input"], entry "input" $ If (Call ">" [value, Number 0])
        (ret (Number 0)) (ret (Call "-" [value, Number 1])))
    , ("constantOrM", ["input"], entry "input" $ If (Call ">" [value, Number 0])
        (ret (Call "+" [value, Number 1])) (ret (Number 0)))
    , ("nestedM", ["input"], entry "input" $
        If (Call ">" [Call "+" [value, Number 1], Number 0])
            (ret (Call "+" [value, Number 2])) (ret value))
    , ("viaCaseM", ["input"], entry "input" (ret (Call "+" [value, value])))
    , ("caseRowsM", ["input", "unused"], entry "input" $ If (Call ">" [value, Number 0])
        (ret (Number 0)) (ret (Call "-" [value, Number 1])))
    , ("coveredRowsM", ["input", "optional"], Block [tick, Bind "tag" (force "optional")] $
        Case (Use "tag")
            [ ("M.JustT", ["_"], Block [Bind "scalar" (force "input")] $
                If (Call ">" [value, Number 0]) (ret (Number 0)) (ret (Call "-" [value, Number 1])))
            , ("M.NothingT", [], ret (Number 1))
            ])
    , ("equationM", ["first", "second"], entry "first" $ If (Call ">" [value, Number 0])
        (force "second") (Block [Bind "other" (force "second")] (ret (Call "-" [value, Use "other"]))))
    , ("checkedFlagM", ["flag"], entry "flag" $ If value
        (ret (Call "==" [value, Call "True" []])) (ret (Call "False" [])))
    , ("givenAddM", ["input"], Block [tick, Bind "field" (suspend givenSum)]
        (ret (Call "M.JustT" [Use "field"])))
    ]
  where
    value = Use "scalar"
    ret e = Call "pure" [e]
    tick = Run (Call "M.tick" [])
    suspend e = Call "M.lazy" [e]
    force thunk = Call "M.force" [Use thunk]
    nil = ret (Call "L.NilT" [])
    entry thunk = Block [tick, Bind "scalar" (force thunk)]
    box = entry "input" $ If (Call ">" [value, Number 0])
        (field "+" "workM" [] "M.JustT" []) (ret (Call "M.NothingT" []))
    -- The expression signature supplies a Num Int dictionary. Both operands
    -- keep their thunked path inside the field, including the literal.
    givenSum = Block
        [ Bind "one" (suspend (ret (Number 1)))
        , Bind "lhs" (force "input")
        , Bind "rhs" (force "one")
        ] (ret (Call "+" [Use "lhs", Use "rhs"]))
    -- Both suspensions belong to the chosen constructor branch. The argument
    -- uses the dominating guard value; the field itself delays the whole call.
    field op callee forwarded constructor heads = Block
        [ Bind "argument" (suspend (ret (Call op [value, Number 1])))
        , Bind "field" (suspend (Call callee (Use "argument" : forwarded)))
        ] (ret (Call constructor (heads ++ [Use "field"])))

-- The successful branch may demand the original lazy alias. Only the failed
-- comparison must use the scalar already obtained from the input.
failedGuard :: Binding -> Either String ()
failedGuard equation = case (m_pats equation, oneRhs equation >>= block) of
    ([p], Just (steps, final))
        | Just input <- patternVariable p
        , Just (_, s) <- unsnoc steps, BindStmt _ binder action <- unLoc s
        , Just scalar <- patternVariable binder
        , HsIf _ predicate _ fallback <- bareExpression final -> do
            match [(input, "input")] (Call "M.force" [Use "input"]) action
            let scope = [(scalar, "scalar")]
            match scope (Call ">" [Use "scalar", Number 0]) predicate
            match scope (Call "pure" [Call "-" [Use "scalar", Number 1]]) fallback
    _ -> Left "expected a scalar bind followed by the guard test"

-- Inspect the action of the rank-polymorphic root suspension. The ordinary
-- field must suspend the addition itself *inside* the selected branch.
payloadBranch :: Binding -> Either String ()
payloadBranch equation = case (m_pats equation, oneRhs equation) of
    ([p], Just body)
        | Just x <- patternVariable p
        , [action] <- [action | HsApp _ f action <- collectAst body,
                maybe False (named "M.lazy") (expressionVariable f),
                not (null [() | HsForAllTy{} <- collectAst f :: [HsType GhcPs]]),
                Just (_, final) <- [block action], HsIf{} <- [bareExpression final]]
        , Just ([s], final) <- block action
        , BindStmt _ binder force <- unLoc s, Just v <- patternVariable binder
        , HsIf _ cond yes _ <- bareExpression final -> do
            let scope = [(x, "input"), (v, "scalar")]
            match scope (Call "M.force" [Use "input"]) force
            match scope (Call ">" [Use "scalar", Number 0]) cond
            let sites = [() | HsApp _ f arg <- collectAst body,
                        maybe False (named "M.force") (expressionVariable f),
                        expressionVariable arg == Just x]
                adds = [() | HsApp _ f arg <- collectAst yes,
                        maybe False (named "M.lazy") (expressionVariable f),
                        Right () <- [match scope (Call "pure" [Call "+" [Use "scalar", Number 1]]) arg]]
            unless (length sites == 1 && length adds == 1) $
                Left ("payloadM: expected one input force and one suspended scalar addition; got " ++
                      show (length sites, length adds))
    _ -> Left "payloadM: expected a guard in the polymorphic root suspension"

checkEquation :: [String] -> Shape -> Binding -> Either String ()
checkEquation roles shape equation = case (bindPatterns roles (m_pats equation), oneRhs equation) of
    (Just scope, Just body) -> match scope shape body
    _ -> Left "expected named parameters and one unguarded body"

bindPatterns :: [String] -> [LPat GhcPs] -> Maybe Scope
bindPatterns [] [] = Just []
bindPatterns ("_":roles) (L _ (WildPat _):pats) = bindPatterns roles pats
bindPatterns (role:roles) (pat:pats) = do
    name <- patternVariable pat
    ((name, role) :) <$> bindPatterns roles pats
bindPatterns _ _ = Nothing

match :: Scope -> Shape -> Expr -> Either String ()
match scope shape expression = case shape of
    Use role
        | (expressionVariable expression >>= (`lookup` scope)) == Just role -> pure ()
        | otherwise -> expected ("a use of the " ++ role ++ " binder")
    Call name operands
        | let (function, args) = application expression
        , Just actual <- expressionVariable function, named name actual
        , isNothing (lookup actual scope), length args == length operands ->
            zipWithM_ (match scope) operands args
        | otherwise -> expected (if null operands then name else "a saturated call to " ++ name)
    Number n
        | HsOverLit _ OverLit{ol_val = HsIntegral lit} <- bareExpression expression
        , il_value lit == n -> pure ()
        | otherwise -> expected ("literal operand " ++ show n)
    Block steps result
        | Just (statements, final) <- block expression -> checkSteps scope steps statements result final
        | otherwise -> expected "an ordinary do block"
    If predicate yes no
        | HsIf _ test positive negative <- bareExpression expression ->
            match scope predicate test >> match scope yes positive >> match scope no negative
        | otherwise -> expected "an if on the scalar comparison"
    Case subject alternatives
        | HsCase _ actual matches <- bareExpression expression
        , let branches = map unLoc (unLoc (mg_alts matches)), length branches == length alternatives -> do
            match scope subject actual
            forM_ alternatives $ \(constructor, roles, body) -> case
                [(locals, branch) | alternative <- branches, [pat] <- [m_pats alternative],
                    Just (name, fields) <- [constructorPattern pat], named constructor name,
                    Just locals <- [bindPatterns roles fields], Just branch <- [oneRhs alternative]] of
                [(locals, branch)] -> match (locals ++ scope) body branch
                _ -> expected ("one alternative for " ++ constructor)
        | otherwise -> expected "a case on the forced list with both constructor alternatives"
  where
    expected = problem (getLocA expression)

-- Flatten terminal do blocks, but do not descend into an if, case, or argument
-- of M.lazy. Moving a bind across any of those changes the checked structure.
block :: Expr -> Maybe ([ExprLStmt GhcPs], Expr)
block expression = case bareExpression expression of
    HsDo _ (DoExpr Nothing) statements -> do
        (prefix, terminal) <- unsnoc (unLoc statements)
        (rest, final) <- block =<< terminalExpression terminal
        pure (prefix ++ rest, final)
    _ -> Just ([], expression)

checkSteps :: Scope -> [Step] -> [ExprLStmt GhcPs] -> Shape -> Expr -> Either String ()
checkSteps scope [] [] result final = match scope result final
checkSteps scope (step:steps) (s:ss) result final = case (step, unLoc s) of
    (Run action, BodyStmt _ actual _ _) ->
        match scope action actual >> checkSteps scope steps ss result final
    (Bind role action, BindStmt _ pat actual) | Just name <- patternVariable pat ->
        match scope action actual >> checkSteps ((name, role):scope) steps ss result final
    _ -> problem (getLocA s) "the next action or named bind in this block"
checkSteps _ [] (s:_) _ _ = problem (getLocA s) "the block result without another action"
checkSteps _ (_:_) [] _ final = problem (getLocA final) "another action before the block result"

constructorPattern :: LPat GhcPs -> Maybe (RdrName, [LPat GhcPs])
constructorPattern (L _ (ConPat _ name (PrefixCon [] fields))) = Just (unLoc name, fields)
constructorPattern (L _ (ParPat _ pat)) = constructorPattern pat
constructorPattern (L _ (SigPat _ pat _)) = constructorPattern pat
constructorPattern _ = Nothing

problem :: SrcSpan -> String -> Either String a
problem span' wanted = Left (showSDocUnsafe (ppr span') ++ ": expected " ++ wanted)
