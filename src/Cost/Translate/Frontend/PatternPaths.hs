module Cost.Translate.Frontend.PatternPaths
    ( plannedFieldKeys
    , plannedMatchKeys
    , patternRoots
    ) where

import Cost.Translate.Frontend.Syntax
import Data.Data (Data)
import Data.List (elemIndex)
import Data.Map.Strict qualified as Map
import GHC
import GHC.Types.Basic (Boxity (..))

patternRoots :: (Data a) => a -> [(LPat GhcRn, String)]
patternRoots syntax =
    concat
        [ [ (pattern', patternRoot pattern' index)
          | (index, pattern') <- zip [0 ..] (m_pats match)
          ]
        | L _ match <- collect syntax :: [LMatch GhcRn (LHsExpr GhcRn)]
        ]
        ++ [ (pattern', patternRoot pattern' 0)
           | L _ (BindStmt _ pattern' _) <- collect syntax :: [GuardLStmt GhcRn]
           ]

plannedFieldKeys :: Map.Map Name [Name] -> LPat GhcRn -> String -> [String]
plannedFieldKeys layouts pattern' path = case unLoc pattern' of
    ParPat _ nested -> plannedFieldKeys layouts nested path
    AsPat _ _ nested -> plannedFieldKeys layouts nested path
    TuplePat _ patterns' Boxed -> children patterns'
    ListPat _ patterns' -> listChildren layouts path patterns'
    LitPat _ (HsString _ text) ->
        plannedFieldKeys layouts (stringPattern text) path
    ConPat _ name details -> case details of
        PrefixCon _ patterns' -> children patterns'
        InfixCon left right -> children [left, right]
        RecCon fields ->
            concat
                [ fieldKeys layouts (fieldPath path index) (hfbRHS field)
                | located <- rec_flds fields
                , let field = unLoc located
                , let label = fieldName (unLoc (hfbLHS field))
                , Just labels <- [Map.lookup (unLoc name) layouts]
                , Just index <- [elemIndex label labels]
                ]
    _ -> []
    where
        children patterns' =
            concat
                [ fieldKeys layouts (fieldPath path index) nested
                | (index, nested) <- zip [0 ..] patterns'
                ]

fieldKeys :: Map.Map Name [Name] -> String -> LPat GhcRn -> [String]
fieldKeys layouts path nested =
    [path | needsField nested] ++ plannedFieldKeys layouts nested path

listChildren :: Map.Map Name [Name] -> String -> [LPat GhcRn] -> [String]
listChildren _ _ [] = []
listChildren layouts path (pattern' : patterns') =
    fieldKeys layouts (fieldPath path 0) pattern'
        ++ [fieldPath path 1 | not (null patterns')]
        ++ listChildren layouts (fieldPath path 1) patterns'

needsField :: LPat GhcRn -> Bool
needsField pattern' = case unLoc pattern' of
    VarPat {} -> False
    AsPat {} -> False
    ParPat _ nested -> needsField nested
    WildPat {} -> False
    _ -> True

plannedMatchKeys :: Map.Map Name [Name] -> LPat GhcRn -> String -> [String]
plannedMatchKeys layouts pattern' path = case unLoc pattern' of
    VarPat {} -> []
    WildPat {} -> []
    ParPat _ nested -> plannedMatchKeys layouts nested path
    AsPat _ _ nested -> plannedMatchKeys layouts nested path
    LitPat _ (HsString _ text) ->
        plannedMatchKeys layouts (stringPattern text) path
    TuplePat _ patterns' Boxed -> path : children patterns'
    ListPat _ patterns' -> path : listMatches layouts path patterns'
    ConPat _ constructor details ->
        path : case details of
            PrefixCon _ patterns' -> children patterns'
            InfixCon left right -> children [left, right]
            RecCon fields ->
                concat
                    [ plannedMatchKeys layouts (hfbRHS field) childPath
                    | located <- rec_flds fields
                    , let field = unLoc located
                    , let label = fieldName (unLoc (hfbLHS field))
                    , Just labels <- [Map.lookup (unLoc constructor) layouts]
                    , Just index <- [elemIndex label labels]
                    , let childPath = fieldPath path index
                    ]
    _ -> [path]
    where
        children patterns' =
            concat
                [ plannedMatchKeys layouts nested (fieldPath path index)
                | (index, nested) <- zip [0 ..] patterns'
                ]

listMatches :: Map.Map Name [Name] -> String -> [LPat GhcRn] -> [String]
listMatches _ _ [] = []
listMatches layouts path (pattern' : patterns') =
    plannedMatchKeys layouts pattern' (fieldPath path 0)
        ++ [fieldPath path 1 | not (null patterns')]
        ++ listMatches layouts (fieldPath path 1) patterns'
