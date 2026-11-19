-- | The generated language is finite. All declarations and all local bodies,
-- including unused ones, start in the construction role.
module ConstructionSyntax (checkSyntax) where

import ConstructionAst
import ConstructionRuntime
import Data.Data (Data, cast)
import GHC
import GHC.Types.Name (isDataConName, isVarName, isFieldName, nameModule_maybe)

checkSyntax :: Runtime -> [Module] -> [Name] -> [Name] -> [(SrcSpan, Name)] -> HsGroup GhcRn -> ([String], [(SrcSpan, Name)])
checkSyntax rt homes methods restricted support = walk noSrcSpan
  where
    walk :: Data a => SrcSpan -> a -> ([String], [(SrcSpan, Name)])
    walk location value
        | Just (_ :: HsType GhcRn) <- cast value = ([], [])
        | Just (_ :: Sig GhcRn) <- cast value = ([], [])
        | Just (_ :: HsMatchContext (LIdP GhcRn)) <- cast value = ([], [])
        | Just (FunBind _ name matches) <- cast value :: Maybe (HsBind GhcRn) =
            if (getLocA name, unLoc name) `elem` support then ([], [])
            else walk (getLocA name) matches `plus`
                case roleOf rt (unLoc name) of
                    Just role | role `elem` [AbstractMethod, FullMethod, UsageMethod, DemandArbitraryMethod] ->
                        ([pretty (getLocA name) ++ ": unsupported observation schema for " ++ describeName (unLoc name)], [])
                    _ -> ([], [])
        | Just (PatSynBind _ _) <- cast value :: Maybe (HsBind GhcRn) =
            ([pretty location ++ ": unsupported executable form: pattern synonym"], [])
        | Just expression <- cast value :: Maybe (LHsExpr GhcRn) =
            let here = getLocA expression
                form = unLoc expression
                shape = if expressionForm form then ([], []) else
                    ([pretty here ++ ": unsupported executable expression: " ++ pretty form], [])
                site = case form of HsVar _ name | unLoc name `elem` methods -> ([], [(here, unLoc name)]); _ -> ([], [])
                body = case form of
                    RecordUpd { rupd_expr = input, rupd_flds = RegularRecUpdFields _ fields } ->
                        walk here input `plus` foldr plus ([], [])
                            [walk here (hfbRHS (unLoc field)) `plus` case unLoc (hfbLHS (unLoc field)) of
                                Unambiguous name _ -> walk here name
                                Ambiguous{} -> ([], []) | field <- fields]
                    RecordUpd{} -> ([pretty here ++ ": unsupported executable record update"], [])
                    _ -> descend here expression
            in shape `plus` site `plus` body
        | Just pattern' <- cast value :: Maybe (LPat GhcRn) =
            (if patternForm (unLoc pattern') then ([], []) else
                ([pretty (getLocA pattern') ++ ": unsupported executable pattern: " ++ pretty pattern'], [])) `plus` descend (getLocA pattern') pattern'
        | Just (_ :: ForeignDecl GhcRn) <- cast value = bad "foreign declaration"
        | Just (_ :: RuleDecl GhcRn) <- cast value = bad "rewrite rule"
        | Just (_ :: AnnDecl GhcRn) <- cast value = bad "annotation splice"
        | Just name <- cast value :: Maybe Name, isVarName name || isDataConName name || isFieldName name =
            if permitted name then ([], []) else
                ([pretty location ++ ": unapproved " ++ (if runtimeOwns rt name then "runtime" else "external") ++
                  " operation " ++ describeName name ++ " is forbidden in generated code"], [])
        | otherwise = descend location value
      where
        bad form = ([pretty location ++ ": unsupported executable form: " ++ form], [])
    descend location value = foldr plus ([], []) (children (walk location) value)
    plus (a, b) (c, d) = (a ++ c, b ++ d)
    permitted name | name `elem` restricted = False
    permitted name = case nameModule_maybe name of
        Nothing -> True
        Just owner | owner `elem` homes -> True
        _ -> name `elem` map snd (standardNames rt) || case roleOf rt name of
            Just Primitive -> True
            Just Derived -> True
            Just Representation -> True
            Just DemandData -> True
            Just ReprMethod -> True
            _ -> False

expressionForm :: HsExpr GhcRn -> Bool
expressionForm expression = case expression of
    HsVar{} -> True
    HsRecSel{} -> True
    HsUnboundVar{} -> False
    HsLit{} -> True
    HsOverLit{} -> True
    HsLam{} -> True
    HsApp{} -> True
    HsAppType{} -> True
    OpApp{} -> True
    NegApp{} -> True
    HsPar{} -> True
    SectionL{} -> True
    SectionR{} -> True
    ExplicitTuple{} -> True
    HsCase{} -> True
    HsIf{} -> True
    HsMultiIf{} -> True
    HsLet{} -> True
    HsDo{} -> True
    ExplicitList{} -> True
    RecordCon{} -> True
    RecordUpd{} -> True
    ExprWithTySig{} -> True
    XExpr{} -> True
    _ -> False

patternForm :: Pat GhcRn -> Bool
patternForm pattern' = case pattern' of
    WildPat{} -> True
    VarPat{} -> True
    LazyPat{} -> True
    AsPat{} -> True
    ParPat{} -> True
    BangPat{} -> True
    ListPat{} -> True
    TuplePat{} -> True
    ConPat{} -> True
    LitPat{} -> True
    NPat{} -> True
    SigPat{} -> True
    _ -> False
