module Cost.Translate.Frontend.Match.Paths
    ( plannedFieldKeys
    , plannedMatchKeys
    , patternRoots
    ) where

import Cost.Translate.Frontend.Match.Pattern
import Cost.Translate.Frontend.Syntax
import Data.Data (Data)
import Data.Map.Strict qualified as Map
import GHC

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
plannedFieldKeys layouts pat path =
    [sourcePath p | p <- drop 1 (nodes layouts pat path), needsField p]

plannedMatchKeys :: Map.Map Name [Name] -> LPat GhcRn -> String -> [String]
plannedMatchKeys layouts pat path =
    [sourcePath p | p <- nodes layouts pat path, inspected (test p)]
    where
        inspected Skip = False
        inspected _ = True

nodes :: Map.Map Name [Name] -> LPat GhcRn -> String -> [Pattern]
nodes layouts pat path =
    either (const []) flatten (patternAt (`Map.lookup` layouts) pat path)
    where
        flatten p =
            p : case test p of
                Deconstruct _ fields -> concatMap (flatten . snd) fields
                _ -> []
