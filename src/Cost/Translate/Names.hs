module Cost.Translate.Names (generatedName, demandHelperName, sourceConstructor) where

import GHC.Types.Name (Name, getOccString)
import Data.Char (isAlpha, isAlphaNum, ord)

generatedName :: String -> Name -> String
generatedName suffix name = case getOccString name of
    -- Unlike the ordinary T/D suffixes, this spelling cannot collide with any
    -- other source-derived representation name. FoldableT is the fixed helper.
    "Foldable" | suffix == "T" -> "FoldableRepresentation"
    s | simpleIdentifier s -> s ++ suffix
    -- Symbolic names end in a decimal code, whereas identifier translations end
    -- in M/T/D. The namespaces are disjoint even when source identifiers look
    -- exactly like an encoded operator. T and D also stay distinct from each other.
    s -> (if suffix == "M" then "operator" else "Constructor") ++ suffix ++ encode s

demandHelperName :: String -> Name -> String
demandHelperName prefix name = case getOccString name of
    s | simpleIdentifier s -> prefix ++ s ++ "Demand"
    s -> prefix ++ "Operator" ++ encode s

encode :: String -> String
encode = concatMap (("_" ++) . show . ord)

simpleIdentifier :: String -> Bool
simpleIdentifier (c:cs) = (isAlpha c || c == '_') && all (\x -> isAlphaNum x || x `elem` "_'") cs
simpleIdentifier [] = False

sourceConstructor :: Name -> String
sourceConstructor name = "Source." ++ getOccString name
