module Cost.Translate.Frontend.Syntax
    ( collect
    , identity
    , builtin
    , arity
    , firstPatterns
    , fieldName
    , actionKey
    , patternRoot
    , fieldPath
    , stringPattern
    , argumentKey
    , explicitScopes
    , plainRhs
    ) where

import Data.Data (Data, cast, gmapQ)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import GHC
import GHC.Data.FastString (FastString, unpackFS)
import GHC.Types.Name (getOccString, nameModule_maybe)
import GHC.Types.SourceText (SourceText (NoSourceText))

collect :: (Data a, Data b) => b -> [a]
collect x = maybeToList (cast x) ++ concat (gmapQ collect x)

plainRhs :: Match GhcRn (LHsExpr GhcRn) -> Maybe (LHsExpr GhcRn)
plainRhs match = case m_grhss match of
    GRHSs _ [L _ (GRHS _ [] expression)] (EmptyLocalBinds _) -> Just expression
    _ -> Nothing

explicitScopes :: HsGroup GhcRn -> Map.Map Name [Name]
explicitScopes group =
    Map.fromList
        [ (unLoc name, map hsLTyVarName binders)
        | L _ (TypeSig _ names signature) <- collect group :: [LSig GhcRn]
        , HsOuterExplicit _ binders <- [sig_bndrs (unLoc (hswc_body signature))]
        , name <- names
        ]

identity :: Name -> String
identity n =
    maybe "local" (moduleNameString . moduleName) (nameModule_maybe n)
        ++ "."
        ++ getOccString n

builtin :: Name -> String -> String -> Bool
builtin name modName occurrence =
    getOccString name == occurrence
        && maybe
            False
            ((== modName) . moduleNameString . moduleName)
            (nameModule_maybe name)

arity :: MatchGroup GhcRn (LHsExpr GhcRn) -> Int
arity matches = length (firstPatterns matches)

firstPatterns :: MatchGroup GhcRn (LHsExpr GhcRn) -> [LPat GhcRn]
firstPatterns matches = case unLoc (mg_alts matches) of
    L _ match : _ -> m_pats match
    [] -> []

fieldName :: FieldOcc GhcRn -> Name
fieldName = foExt

actionKey :: String -> SrcSpan -> String
actionKey prefix (RealSrcSpan location _) =
    prefix
        ++ concatMap
            (("_" ++) . show)
            [ srcSpanStartLine location
            , srcSpanStartCol location
            , srcSpanEndLine location
            , srcSpanEndCol location
            ]
actionKey prefix _ = prefix ++ "_generated"

patternRoot :: LPat GhcRn -> Int -> String
patternRoot pattern' index =
    actionKey "pattern" (getLocA pattern') ++ "_" ++ show index

argumentKey :: LHsExpr GhcRn -> String
argumentKey expression = actionKey "argument" (getLocA expression)

fieldPath :: String -> Int -> String
fieldPath path index = path ++ "_field" ++ show index

stringPattern :: FastString -> LPat GhcRn
stringPattern text =
    noLocA
        ( ListPat
            noExtField
            (map character (unpackFS text))
        )
    where
        character = noLocA . LitPat noExtField . HsChar NoSourceText
