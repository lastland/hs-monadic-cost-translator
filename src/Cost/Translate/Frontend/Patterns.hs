module Cost.Translate.Frontend.Patterns
    ( matchPatterns
    , constructorFields
    , adjacentConstructors
    , exhaustiveConstructors
    ) where

import Cost.Translate.Datatypes (constructors)
import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.Gates (locatedAt)
import Cost.Translate.Frontend.NamePlan (NamePlan (..))
import Cost.Translate.Frontend.Resolve
import Cost.Translate.Frontend.Syntax
import Cost.Translate.Output qualified as O
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import GHC
import GHC.Types.Basic (Boxity (..))
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

adjacentConstructors ::
    Env ->
    [LMatch GhcRn (LHsExpr GhcRn)] ->
    ([LMatch GhcRn (LHsExpr GhcRn)], [LMatch GhcRn (LHsExpr GhcRn)])
adjacentConstructors env = go Set.empty
    where
        go seen (located@(L _ match) : rest)
            | [pattern'] <- m_pats match
            , Just con <- patternConstructor env pattern'
            , con `Set.notMember` seen =
                let (group, remaining) = go (Set.insert con seen) rest
                 in (located : group, remaining)
        go _ rest = ([], rest)

patternConstructor :: Env -> LPat GhcRn -> Maybe String
patternConstructor env (L _ pattern') = case pattern' of
    ParPat _ nested -> patternConstructor env nested
    AsPat _ _ nested -> patternConstructor env nested
    TuplePat _ [_, _] Boxed -> Just "M.PairT"
    ListPat _ [] -> Just listNil
    ListPat {} -> Just listCons
    ConPat _ name details -> case details of
        PrefixCon {} -> resolvedConstructor env (unLoc name)
        InfixCon {} -> resolvedConstructor env (unLoc name)
        RecCon {}
            | Just Constructor {} <- lookupEnv (unLoc name) env ->
                resolvedConstructor env (unLoc name)
        _ -> Nothing
    _ -> Nothing

exhaustiveConstructors :: Env -> [String] -> Bool
exhaustiveConstructors env heads = any (all (`elem` heads)) families
    where
        families =
            [[listNil, listCons], ["M.NothingT", "M.JustT"], ["False", "True"]]
                ++ [ cons
                   | tycon <- sourceDatatypes env
                   , let cons = [con | (_, con, _, _) <- constructors [tycon]]
                   , not (null cons)
                   ]

matchPatterns ::
    Env ->
    [(O.Expr, LPat GhcRn)] ->
    (Env -> Either String O.Expr) ->
    O.Expr ->
    Either String O.Expr
matchPatterns env patterns =
    matchPatternsAt
        env
        [ (thunk, pattern', patternRoot pattern' index)
        | (index, (thunk, pattern')) <- zip [0 :: Int ..] patterns
        ]

matchPatternsAt ::
    Env ->
    [(O.Expr, LPat GhcRn, String)] ->
    (Env -> Either String O.Expr) ->
    O.Expr ->
    Either String O.Expr
matchPatternsAt env [] success _ = success env
matchPatternsAt env ((thunk, located, path) : rest) success failure =
    case unLoc located of
        VarPat _ name -> continue (insertEnv (unLoc name) (Thunk thunk) env)
        WildPat _ -> continue env
        ParPat _ pat ->
            matchPatternsAt env ((thunk, pat, path) : rest) success failure
        AsPat _ name pat ->
            matchPatternsAt
                (insertEnv (unLoc name) (Thunk thunk) env)
                ((thunk, pat, path) : rest)
                success
                failure
        TuplePat _ [_, _] Boxed -> constructorCase
        ListPat {} -> constructorCase
        NPat _ literal negative _ -> do
            body <- continue env
            let positive = O.overLit (ol_val (unLoc literal))
                value
                    | isJust negative = O.call "negate" [positive]
                    | otherwise = positive
            pure (literalCase value body)
        LitPat _ (HsChar _ c) -> literalCase (O.char c) <$> continue env
        LitPat _ (HsString _ text) ->
            matchPatternsAt
                env
                ((thunk, stringPattern text, path) : rest)
                success
                failure
        ConPat {} -> constructorCase
        _ -> patternError located
    where
        continue scope = matchPatternsAt scope rest success failure
        literalCase value body =
            O.doE
                [O.bind matchName (O.call "M.force" [thunk])]
                (O.ifE (O.op "==" (O.v matchName) value) body failure)
        constructorCase = do
            (con, fields, body) <-
                constructorFields env thunk located path continue failure
            pure
                $ O.doE [O.bind matchName (O.call "M.force" [thunk])]
                $ O.caseE
                    (O.v matchName)
                    ( (O.conP con (map O.p fields), body)
                        : [(O.p "_", failure) | needsFallback con]
                    )
        matchName = Map.findWithDefault "matched" path matches
        matches = plannedMatches (namePlan env)
        needsFallback con = con `Set.notMember` singleConstructors env

constructorFields ::
    Env ->
    O.Expr ->
    LPat GhcRn ->
    String ->
    (Env -> Either String O.Expr) ->
    O.Expr ->
    Either String (String, [String], O.Expr)
constructorFields env thunk pat path success failure = case unLoc pat of
    ParPat _ nested -> constructorFields env thunk nested path success failure
    AsPat _ name nested ->
        constructorFields
            (insertEnv (unLoc name) (Thunk thunk) env)
            thunk
            nested
            path
            success
            failure
    TuplePat _ [a, b] Boxed -> constructor "M.PairT" [a, b]
    ListPat _ [] -> constructor listNil []
    ListPat ext (p : ps) ->
        constructor listCons [p, L (getLoc pat) (ListPat ext ps)]
    ConPat _ name details -> do
        con <-
            maybe
                ( Left
                    ( showSDocUnsafe (ppr (unLoc name))
                        ++ ": constructor pattern not implemented yet"
                    )
                )
                Right
                (resolvedConstructor env (unLoc name))
        case details of
            PrefixCon _ pats -> constructor con pats
            InfixCon a b -> constructor con [a, b]
            RecCon fields
                | Just (Constructor _ _ labels) <- lookupEnv (unLoc name) env ->
                    recordFields env pat path con labels fields success failure
            _ -> Left "unsupported record pattern"
    _ -> patternError pat
    where
        constructor con pats = do
            let paths = [fieldPath path i | i <- [0 .. length pats - 1]]
                fields = zipWith (fieldSlot env) paths pats
                patterns' = zip3 (map O.v fields) pats paths
            body <- matchPatternsAt env patterns' success failure
            pure (con, fields, body)

recordFields ::
    Env ->
    LPat GhcRn ->
    String ->
    String ->
    [Name] ->
    HsRecFields GhcRn (LPat GhcRn) ->
    (Env -> Either String O.Expr) ->
    O.Expr ->
    Either String (String, [String], O.Expr)
recordFields env located path con labels fields success failure = do
    let slots = recordSlots env path labels fields
    ordered <- mapM (orderField slots) (rec_flds fields)
    body <-
        matchPatternsAt
            env
            [(O.v slot, pat, field) | (slot, pat, field) <- ordered]
            success
            failure
    let used = [slot | (slot, pat, _) <- ordered, not (wildcard pat)]
    pure (con, [if slot `elem` used then slot else "_" | slot <- slots], body)
    where
        orderField slots locatedField = do
            let field = unLoc locatedField
            let label = fieldName (unLoc (hfbLHS field))
                layout = zip labels (zip slots [0 ..])
            (slot, index) <-
                locatedAt (getLocA located) $
                    maybe
                        (Left "unknown record pattern field")
                        Right
                        (lookup label layout)
            pure (slot, hfbRHS field, fieldPath path index)
        wildcard pat = case unLoc pat of
            WildPat _ -> True
            ParPat _ nested -> wildcard nested
            _ -> False

recordSlots ::
    Env -> String -> [Name] -> HsRecFields GhcRn (LPat GhcRn) -> [String]
recordSlots env path labels fields =
    [ fieldSlot env (fieldPath path i) pattern'
    | (i, pattern') <-
        zip
            [0 ..]
            [ maybe
                (noLocA (WildPat noExtField))
                (hfbRHS . unLoc)
                ( lookup
                    label
                    [ (fieldName (unLoc (hfbLHS (unLoc f))), f)
                    | f <- rec_flds fields
                    ]
                )
            | label <- labels
            ]
    ]

patternError :: LPat GhcRn -> Either String a
patternError located =
    locatedAt (getLocA located) (Left "pattern not implemented yet")

listNil :: String
listNil = "L.NilT"

listCons :: String
listCons = "L.ConsT"
