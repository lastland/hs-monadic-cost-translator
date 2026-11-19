-- | Narrow, declaration-specific observation roles. These are not exemptions
-- for pure functions: an unrecognized body is checked as construction code.
module ConstructionRoles (supportBindings, supportCoreNames) where

import ConstructionAst
import ConstructionRuntime
import Data.Maybe (mapMaybe)
import GHC

supportBindings :: Runtime -> [Name] -> HsGroup GhcRn -> [(SrcSpan, Name)]
supportBindings rt constructors group =
    [(getLocA name, unLoc name) | binding@(FunBind _ name _) <- collectAst group :: [HsBind GhcRn],
        case roleOf rt (unLoc name) of
            Just AbstractMethod -> exactAbstract binding
            Just FullMethod -> exactFull binding
            Just UsageMethod -> exactUsage binding
            _ -> False]
  where
    names = roleNames rt
    exactAbstract binding = allEquations binding $ \pats body -> case pats of
        [pat] | Just input <- plainPattern pat, variable body == Just input -> True
        [pat] | Just fields <- fieldsOf pat, Just (_, arguments) <- dataCall body, length fields == length arguments ->
            and (zipWith abstractField fields arguments)
        _ -> False
    abstractField field expression = case call (names AbstractMethod) expression of
        Just [projected] -> case call (names Payload) projected of
            Just [input] -> variable input == Just field
            _ -> False
        _ -> False
    exactFull binding = allEquations binding $ \pats body -> case pats of
        [pat] | Just input <- plainPattern pat, variable body == Just input -> True
        [pat] | Just fields <- fieldsOf pat, Just (_, arguments) <- dataCall body, length fields == length arguments ->
            and (zipWith (\field expression -> case call (names FullMethod) expression of
                Just [input] -> variable input == Just field
                _ -> False) fields arguments)
        _ -> False
    exactUsage binding = allEquations binding $ \pats body -> case pats of
        [pat, demand] | Just fields <- fieldsOf pat, Just demands <- fieldsOf demand, length fields == length demands ->
            accounting (zip fields demands) body
        [L _ (WildPat _), L _ (WildPat _)] -> variable body `elem` map Just (names EmptyAccounting)
        _ -> False
    accounting [] expression = variable expression `elem` map Just (names EmptyAccounting)
    accounting ((field, demand) : rest) expression = case unLoc expression of
        OpApp _ left operator right | variable operator `elem` map Just (names UnionAccounting) ->
            case call (names UsageMethod) left of
                Just [input, demanded] -> variable input == Just field && variable demanded == Just demand && accounting rest right
                _ -> False
        _ -> False
    fieldsOf (L _ (ParPat _ pat)) = fieldsOf pat
    fieldsOf (L _ (ConPat _ constructor (PrefixCon [] pats)))
        | unLoc constructor `elem` constructors = mapM plainPattern pats
    fieldsOf _ = Nothing
    dataCall expression = case spine expression of
        (L _ (HsVar _ constructor), arguments) | unLoc constructor `elem` constructors -> Just (unLoc constructor, arguments)
        _ -> Nothing

allEquations :: HsBind GhcRn -> ([LPat GhcRn] -> LHsExpr GhcRn -> Bool) -> Bool
allEquations binding valid = case equations binding of
    Just cases -> not (null cases) && all (uncurry valid) cases
    Nothing -> False

-- GHC gives instance methods fresh internal identifiers. Follow only the finite
-- AbsBinds export relation, not name spellings or IdInfo/unfoldings.
supportCoreNames :: [(SrcSpan, Name)] -> TypecheckedModule -> [Name]
supportCoreNames support checked = close initial
  where
    initial = [getName (unLoc name) | FunBind _ name _ <- collectAst (tm_typechecked_source checked) :: [HsBind GhcTc],
        getLocA name `elem` map fst support]
    exports = [(getName poly, getName mono) | ABE poly mono _ _ <- collectAst (tm_typechecked_source checked) :: [ABExport]]
    close names = let more = names ++ mapMaybe (\(poly, mono) -> if mono `elem` names && poly `notElem` names then Just poly else Nothing) exports
                  in if length more == length names then names else close more
