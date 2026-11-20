module Cost.Translate.Frontend.Patterns (matchRows, matchPatterns) where

import Cost.Translate.Datatypes (constructors)
import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.Match.Fallback (withFallback)
import Cost.Translate.Frontend.Match.Row
import Cost.Translate.Output qualified as O
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import GHC

matchPatterns ::
    Env ->
    [(O.Expr, LPat GhcRn)] ->
    (Env -> Result) ->
    O.Expr ->
    Result
matchPatterns env pats success =
    matchRows env [(noSrcSpan, pats, \scope _ -> success scope)]

matchRows ::
    Env -> [(SrcSpan, [(O.Expr, LPat GhcRn)], Body)] -> O.Expr -> Result
matchRows env equations failure = do
    rows <- mapM (prepareRow env) equations
    compileRows env rows failure

compileRows :: Env -> [Row] -> O.Expr -> Result
compileRows _ [] failure = Right failure
compileRows env (Row name step : rows) failure = case step of
    Ready finish -> following name rows finish
    Inspect subject check ->
        let (neighbors, rest) = spanMap (sameSubject subject) rows
            checks = (name, check) :| neighbors
            matched = matchedName subject
            force = O.bind matched (O.call "M.force" [reference subject])
            finish =
                fmap (O.doE [force])
                    . compileChecks env (O.v matched) (NE.toList checks)
         in following (fst (NE.last checks)) rest finish
    where
        following = withRest (compileRows env) failure
        sameSubject subject (Row next (Inspect other check))
            | position subject == position other = Just (next, check)
        sameSubject _ _ = Nothing

compileChecks :: Env -> O.Expr -> [(String, Check)] -> O.Expr -> Result
compileChecks _ _ [] failure = Right failure
compileChecks env value ((name, Compare expected advance) : rest) failure =
    withRest (compileChecks env value) failure name rest $ \miss -> do
        row <- advance
        yes <- compileRows env [row] miss
        pure (O.ifE (O.op "==" value expected) yes miss)
compileChecks env value ((name, Choose first) : checks) failure =
    let (neighbors, rest) = spanMap asConstructor checks
        choices = (name, first) :| neighbors
        group = map snd (NE.toList choices)
        complete = exhaustive env (map tag group)
        following = withRest (compileChecks env value) failure
     in following (fst (NE.last choices)) rest $ \miss -> do
            branches <- alternatives env group miss
            pure (O.caseE value (branches ++ [(O.p "_", miss) | not complete]))
    where
        asConstructor (next, Choose branch) = Just (next, branch)
        asConstructor _ = Nothing

alternatives :: Env -> [Branch] -> O.Expr -> Either String [(O.Pat, O.Expr)]
alternatives _ [] _ = Right []
alternatives env (first : branches) failure = do
    let (same, other) = partition ((== tag first) . tag) branches
    branch <- emitBranch (first :| same) (\rows -> compileRows env rows failure)
    (branch :) <$> alternatives env other failure

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
