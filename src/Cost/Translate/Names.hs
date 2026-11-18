module Cost.Translate.Names
    ( generatedName
    , demandHelperName
    , sourceConstructor
    ) where

import Data.Char (isAlpha, isAlphaNum, ord)
import GHC.Types.Name (Name, getOccString)

generatedName :: String -> Name -> String
generatedName suffix name = case getOccString name of
    "Foldable" | suffix == "T" -> "FoldableRepresentation"
    s | simpleIdentifier s -> s ++ suffix
    s ->
        (if suffix == "M" then "operator" else "Constructor")
            ++ suffix
            ++ encode s

demandHelperName :: String -> Name -> String
demandHelperName prefix name = case getOccString name of
    s | simpleIdentifier s -> prefix ++ s ++ "Demand"
    s -> prefix ++ "Operator" ++ encode s

encode :: String -> String
encode = concatMap (("_" ++) . show . ord)

simpleIdentifier :: String -> Bool
simpleIdentifier (c : cs) =
    (isAlpha c || c == '_') && all (\x -> isAlphaNum x || x `elem` "_'") cs
simpleIdentifier [] = False

sourceConstructor :: Name -> String
sourceConstructor name = "Source." ++ getOccString name
