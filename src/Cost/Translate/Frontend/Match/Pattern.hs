module Cost.Translate.Frontend.Match.Pattern
    ( Pattern (..)
    , Test (..)
    , Head (..)
    , patternAt
    , needsField
    ) where

import Cost.Translate.Frontend.Gates (locatedAt)
import Cost.Translate.Frontend.Syntax (fieldName, fieldPath, stringPattern)
import Cost.Translate.Output qualified as O
import Data.List (elemIndex)
import Data.Maybe (isJust)
import GHC
import GHC.Types.Basic (Boxity (..))

data Pattern = Pattern
    { sourcePath :: String
    , aliases :: [Name]
    , test :: Test
    }

data Test
    = Skip
    | Equal O.Expr
    | Deconstruct Head [(Int, Pattern)]

data Head = Builtin String | Named Name

needsField :: Pattern -> Bool
needsField (Pattern _ [] Skip) = False
needsField _ = True

patternAt ::
    (Name -> Maybe [Name]) -> LPat GhcRn -> String -> Either String Pattern
patternAt labels located path = view [] located
    where
        view names pat = case unLoc pat of
            VarPat _ name -> finish (unLoc name : names) Skip
            WildPat _ -> finish names Skip
            ParPat _ nested -> view names nested
            AsPat _ name nested -> view (unLoc name : names) nested
            TuplePat _ [a, b] Boxed -> positional (Builtin "M.PairT") [a, b]
            ListPat _ [] -> positional (Builtin "L.NilT") []
            ListPat ext (p : ps) ->
                let tailPat = L (getLoc pat) (ListPat ext ps)
                 in positional (Builtin "L.ConsT") [p, tailPat]
            LitPat _ (HsString _ text) -> view names (stringPattern text)
            LitPat _ (HsChar _ c) -> finish names (Equal (O.char c))
            NPat _ literal negative _ ->
                let value = O.overLit (ol_val (unLoc literal))
                    signed
                        | isJust negative = O.call "negate" [value]
                        | otherwise = value
                 in finish names (Equal signed)
            ConPat _ name details -> case details of
                PrefixCon _ ps -> positional head' ps
                InfixCon a b -> positional head' [a, b]
                RecCon fields -> do
                    let missing = Left "unsupported record pattern"
                    layout <- maybe missing Right (labels source)
                    ordered <- mapM (recordField layout pat) (rec_flds fields)
                    constructor head' ordered
                where
                    source = unLoc name
                    head' = Named source
            _ -> locatedAt (getLocA pat) (Left "pattern not implemented yet")
            where
                positional head' = constructor head' . zip [0 ..]
                constructor head' fields =
                    finish names . Deconstruct head' =<< mapM child fields
        finish names = Right . Pattern path names
        child (index, pat) = do
            nested <- patternAt labels pat (fieldPath path index)
            pure (index, nested)

recordField ::
    [Name] ->
    LPat GhcRn ->
    LHsRecField GhcRn (LPat GhcRn) ->
    Either String (Int, LPat GhcRn)
recordField labels pat (L _ field) =
    let label = fieldName (unLoc (hfbLHS field))
     in locatedAt (getLocA pat) $ case elemIndex label labels of
            Nothing -> Left "unknown record pattern field"
            Just index -> Right (index, hfbRHS field)
