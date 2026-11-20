module Cost.Translate.Frontend.Patterns
    ( GuardEnd
    , matchGuard
    , matchesWith
    ) where

import Cost.Translate.Datatypes (constructors)
import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.Expression.Thunks (failureExpr)
import Cost.Translate.Frontend.Expression.Values (knownValue)
import Cost.Translate.Frontend.Match.Facts
import Cost.Translate.Frontend.Match.Fallback (withFallback)
import Cost.Translate.Frontend.Match.Row
import Cost.Translate.Output qualified as O
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC

type End = Facts -> (O.Expr -> Result) -> Result

type GuardEnd = Env -> (O.Expr -> Result) -> Result

matchesWith ::
    (Env -> Match GhcRn (LHsExpr GhcRn) -> O.Expr -> Result) ->
    Env ->
    Maybe (LHsExpr GhcRn) ->
    [O.Expr] ->
    MatchGroup GhcRn (LHsExpr GhcRn) ->
    Result
matchesWith body env original args matches =
    matchRows env original equations $ \_ build ->
        build (failureExpr "source pattern match failure")
    where
        equations =
            [ (getLocA m, zip args (m_pats row), (`body` row))
            | m@(L _ row) <- unLoc (mg_alts matches)
            ]

matchRows ::
    Env ->
    Maybe (LHsExpr GhcRn) ->
    [(SrcSpan, [(O.Expr, LPat GhcRn)], Body)] ->
    GuardEnd ->
    Result
matchRows env original equations end = do
    let sources = maybe [] (scrutinee env) original
        known = rootFacts env original sources
    rows <- mapM (prepareRow env sources) equations
    underRows env known rows (\facts -> end (refine facts sources env))

rootFacts :: Env -> Maybe (LHsExpr GhcRn) -> Sources -> Facts
rootFacts env original sources = case original >>= knownValue env of
    Just (_, v) | not (null sources) -> Map.singleton [0] v
    _ -> Map.empty

matchGuard ::
    Env ->
    SrcSpan ->
    LHsExpr GhcRn ->
    (O.Expr, LPat GhcRn) ->
    (Env -> GuardEnd -> Result) ->
    GuardEnd ->
    Result
matchGuard env site expr subject success end = do
    let sources = scrutinee env expr
        known = rootFacts env (Just expr) sources
        joined scope miss = success scope (\_ build -> build miss)
    row <- prepareRow env sources (site, [subject], joined)
    case rowStep row of
        -- Binding aliases alone cannot miss. Let the next inspection decide
        -- which facts dominate all paths to the following guard.
        Ready scope _ -> success (scope known) end
        Inspect {} ->
            underRows env known [row] (\facts -> end (refine facts sources env))

compileRows :: Env -> Facts -> [Row] -> O.Expr -> Result
compileRows env facts rows failure =
    underRows env facts rows (\_ finish -> finish failure)

underRows :: Env -> Facts -> [Row] -> End -> Result
underRows _ facts [] end = end facts Right
underRows env facts allRows@(Row _ name step : rows) end = case step of
    Ready scope finish -> following facts name rows (finish (scope facts))
    Inspect subject check ->
        let (neighbors, rest) = spanMap (sameSubject subject) rows
            checks = (name, check) :| neighbors
            matched = matchedName subject
            pos = position subject
            force = O.bind matched (O.call "M.force" [reference subject])
            lastName = fst (NE.last checks)
            test fs value = compileChecks env fs value (NE.toList checks)
            inspected value =
                let fs = Map.insert pos value facts
                 in following fs lastName rest (test fs value)
         in case Map.lookup pos facts of
                Just value -> inspected value
                Nothing
                    | any (Set.member pos . scalarPositions) allRows ->
                        O.doE [force] <$> inspected (O.v matched)
                _ ->
                    following facts lastName rest $
                        fmap (O.doE [force]) . test facts (O.v matched)
    where
        following fs n rest finish = end fs $ \failure ->
            withRest (compileRows env fs) failure n rest finish
        sameSubject subject (Row _ next (Inspect other check))
            | position subject == position other = Just (next, check)
        sameSubject _ _ = Nothing

compileChecks :: Env -> Facts -> O.Expr -> [(String, Check)] -> O.Expr -> Result
compileChecks _ _ _ [] failure = Right failure
compileChecks env fs value ((name, Compare expected advance) : rest) failure =
    withRest (compileChecks env fs value) failure name rest $ \miss -> do
        row <- advance
        yes <- compileRows env fs [row] miss
        pure (O.ifE (O.op "==" value expected) yes miss)
compileChecks env fs value ((name, Choose first) : checks) failure =
    let (neighbors, rest) = spanMap asConstructor checks
        choices = (name, first) :| neighbors
        group = map snd (NE.toList choices)
        complete = exhaustive env (map tag group)
        following = withRest (compileChecks env fs value) failure
     in following (fst (NE.last choices)) rest $ \miss -> do
            branches <- alternatives env fs group miss
            pure (O.caseE value (branches ++ [(O.p "_", miss) | not complete]))
    where
        asConstructor (next, Choose branch) = Just (next, branch)
        asConstructor _ = Nothing

alternatives ::
    Env -> Facts -> [Branch] -> O.Expr -> Either String [(O.Pat, O.Expr)]
alternatives _ _ [] _ = Right []
alternatives env fs (first : branches) failure = do
    let (same, other) = partition ((== tag first) . tag) branches
    let compile rows = compileRows env fs rows failure
    branch <- emitBranch (first :| same) compile
    (branch :) <$> alternatives env fs other failure

withRest ::
    ([a] -> O.Expr -> Result) ->
    O.Expr ->
    String ->
    [a] ->
    (O.Expr -> Result) ->
    Result
withRest _ failure _ [] finish = finish failure
withRest compile failure name rest finish = do
    following <- compile rest failure
    withFallback name following finish

spanMap :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMap project (x : xs)
    | Just y <- project x =
        let (ys, rest) = spanMap project xs in (y : ys, rest)
spanMap _ rest = ([], rest)

exhaustive :: Env -> [String] -> Bool
exhaustive env tags =
    any (`Set.member` singleConstructors env) tags
        || any (all (`elem` tags)) families
    where
        families =
            [ ["L.NilT", "L.ConsT"]
            , ["M.NothingT", "M.JustT"]
            , ["False", "True"]
            ]
                ++ [ cs
                   | tc <- sourceDatatypes env
                   , let cs = [c | (_, c, _, _) <- constructors [tc]]
                   , not (null cs)
                   ]
