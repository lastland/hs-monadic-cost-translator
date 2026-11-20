-- | The generated language is finite. All declarations and all local bodies,
-- including unused ones, start in the construction role.
module ConstructionSyntax (checkCompilerOptions, checkParsedSyntax, checkSyntax) where

import ConstructionActionFields (missingActions, sourceActions)
import ConstructionAst
import ConstructionLocals (Locals, SourceBody(..), SourceSite(..), checkedActions, implicitBodies, nestedSource, quickCheckBindings)
import ConstructionQuickCheckUses (QuickCheckUses, strictPatternAt, valueAt)
import ConstructionRoles (isQuickCheckBinding, quickCheckBody)
import ConstructionRuntime
import ConstructionViews (ViewSupport, moduleViews, restrictedViews)
import Data.Data (Data, cast)
import Data.Foldable (toList)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import GHC
import GHC.Core.ConLike (conLikeName)
import GHC.Driver.Session (xopt)
import qualified GHC.LanguageExtensions.Type as Extension
import GHC.Types.GREInfo (conLikeName_Name)
import GHC.Types.Id.Make (rightSectionName)
import GHC.Types.Name (isDataConName, isVarName, isFieldName, nameModule_maybe)
import GHC.Types.Name.Set (NameSet)
import GHC.Types.Name.Reader (greName)
import GHC.Types.Unique.Set (nonDetEltsUniqSet)

-- Source and selected-evidence certificates require ordinary, successful GHC
-- checking of the same source. Run this for the entire graph before loading it.
checkCompilerOptions :: ModSummary -> [String]
checkCompilerOptions summary =
    [fromMaybe (pretty (ms_mod summary)) original ++ ": unsupported construction compiler options: " ++ reason
    | (reason, active) <-
        [("deferred errors", any (`gopt` options)
            [Opt_DeferTypeErrors, Opt_DeferTypedHoles, Opt_DeferOutOfScopeVariables])
        ,("compiler plugins", not (null (pluginModNames options) && null (externalPluginSpecs options)))
        -- Strict leaves an ordinary VarPat in the typed tree and inserts a Core
        -- case. StrictData can instead hide the force in a constructor wrapper.
        ,("implicit forcing (Strict)", xopt Extension.Strict options)
        ,("implicit forcing (StrictData)", xopt Extension.StrictData options)
        ,("preprocessed source", gopt Opt_Pp options || original /= Just (ms_hspp_file summary))]
    , active]
  where
    options = ms_hspp_opts summary
    original = ml_hs_file (ms_location summary)

-- Renaming expands splices away. Inspect parsed syntax before load can execute
-- them, including splices in types, patterns, annotations and home dependencies.
checkParsedSyntax :: ParsedModule -> [String]
checkParsedSyntax parsed =
    [pretty (getLoc (pm_parsed_source parsed)) ++ ": unsupported executable form: Template Haskell"
    | any expressionTH (collectAst source) ||
      any typeTH (collectAst source) || any patternTH (collectAst source) ||
      not (null (collectAst source :: [SpliceDecl GhcPs])) ||
      not (null (collectAst source :: [AnnDecl GhcPs]))]
  where
    source = pm_parsed_source parsed
    expressionTH :: HsExpr GhcPs -> Bool
    expressionTH HsTypedSplice{} = True
    expressionTH HsUntypedSplice{} = True
    expressionTH HsTypedBracket{} = True
    expressionTH HsUntypedBracket{} = True
    expressionTH _ = False
    typeTH :: HsType GhcPs -> Bool
    typeTH HsSpliceTy{} = True
    typeTH _ = False
    patternTH :: Pat GhcPs -> Bool
    patternTH SplicePat{} = True
    patternTH _ = False

checkSyntax :: Runtime -> Locals -> (TypecheckedModule -> QuickCheckUses) -> [Module] -> [Name] -> ViewSupport -> TypecheckedModule -> ([String], [SourceSite])
checkSyntax rt locals uses homes methods support checked = case tm_renamed_source checked of
    Just (group, _, _, _, _) -> ([pretty at ++ ": no retained action operand" | at <- missingActions actions], []) `plus`
        foldr (plus . walk noSrcSpan) (walk noSrcSpan group) (implicitBodies group ++ sourceActions actions)
    Nothing -> ([pretty (ms_mod (pm_mod_summary (tm_parsed_module checked))) ++ ": GHC did not retain renamed source"], [])
  where
    qc = quickCheckBindings locals
    actions = checkedActions locals checked
    exempt = quickCheckBody qc checked
    qcUses = uses checked
    (views, isViewCall) = moduleViews support checked
    updates = ambiguousUpdateReferences (tm_typechecked_source checked)
    -- Inspect expansions even inside declaration/call certificates. Looking only
    -- at OrigExpr would hide an implicit external hook such as ifThenElse.
    expansionProblems value = ([unsupported expression
        | expression@(L _ (XExpr expansion)) <- collectAst value :: [LHsExpr GhcRn], not (sectionExpansion expansion)], [])
    unsupported expression = pretty (srcSpanStart (getLocA expression)) ++ ": unsupported executable expansion"
    walk :: Data a => SrcSpan -> a -> ([String], [SourceSite])
    walk location value
        -- GHC caches a declaration's free variables on its renamed node. The
        -- actual uses are inside the method bodies, where their roles are known.
        | Just (_ :: NameSet) <- cast value = ([], [])
        | Just (_ :: IPBind GhcRn) <- cast value = ([], [])  -- checked separately, including under exempt RHSs
        -- A declared field introduces a selector; no value is selected here.
        | Just (_ :: ConDeclField GhcRn) <- cast value = ([], [])
        | Just (_ :: HsType GhcRn) <- cast value = ([], [])
        | Just (_ :: Sig GhcRn) <- cast value = ([], [])
        | Just (_ :: HsMatchContext (LIdP GhcRn)) <- cast value = ([], [])
        | Just binding <- cast value, exempt binding, not (strictBinding binding) =
            foldr (plus . nested location) ([], []) (nestedSource locals binding)
        | Just (FunBind _ name matches) <- cast value :: Maybe (HsBind GhcRn) =
             if (getLocA name, unLoc name) `elem` views then expansionProblems matches
             else ([pretty (getLocA name) ++ ": strict binding forces a QuickCheck value outside a generator or property"
                    | strictNullaryRhs matches, strictPatternAt qcUses (getLocA name)], []) `plus`
                 walk (getLocA name) matches
        | Just (PatSynBind _ PSB{psb_id = name}) <- cast value :: Maybe (HsBind GhcRn) =
            ([pretty (getLocA name) ++ ": unsupported executable form: pattern synonym"], [])
        | Just expression <- cast value :: Maybe (LHsExpr GhcRn) =
            let here = getLocA expression
                form = unLoc expression
                shape = if expressionForm form then ([], []) else
                    ([pretty here ++ ": unsupported executable expression: " ++ pretty form], [])
                reference = case form of
                    HsVar _ name -> Just (unLoc name)
                    HsRecSel _ field -> Just (foExt field)
                    _ -> Nothing
                site = ([], [MethodAt here name | Just name <- [reference], name `elem` methods])
                operand = case form of
                    RecordCon{rcon_con = name} -> valueUse (getLocA name) (unLoc name)
                    _ -> maybe ([], []) (valueUse here) reference
                body = case form of
                    XExpr expansion@(ExpandedThingRn (OrigExpr original) _) -> case expression of
                        L annotation _ -> ([unsupported expression | not (sectionExpansion expansion)], []) `plus`
                            walk here (L annotation original :: LHsExpr GhcRn)
                    XExpr{} -> ([pretty here ++ ": unsupported executable expansion"], [])
                    RecordUpd { rupd_expr = input, rupd_flds = RegularRecUpdFields _ fields } ->
                        walk here input `plus` foldr plus ([], [])
                            [walk here (hfbRHS (unLoc field)) `plus` case unLoc (hfbLHS (unLoc field)) of
                                Unambiguous name label -> checkOperation (getLocA label) name
                                Ambiguous _ label -> case nub [n | (loc, n) <- updates, loc == getLocA label] of
                                    [name] -> checkOperation (getLocA label) name
                                    _ -> ([pretty (getLocA label) ++ ": unresolved ambiguous record update selector"], [])
                            | field <- fields]
                    RecordUpd{} -> ([pretty here ++ ": unsupported executable record update"], [])
                    _ -> descend here expression
             in if isViewCall here then expansionProblems expression else shape `plus` site `plus` operand `plus` body
        | Just field <- cast value :: Maybe (HsRecField GhcRn (LPat GhcRn)) = recordField location field
        | Just field <- cast value :: Maybe (HsRecField GhcRn (LHsExpr GhcRn)) = recordField location field
        | Just pattern' <- cast value :: Maybe (LPat GhcRn) =
            let here = getLocA pattern'
                problems = case unLoc pattern' of
                    BangPat{} | strictPatternAt qcUses here -> [pretty here ++ ": strict pattern forces a QuickCheck value outside a generator or property"]
                    form -> [pretty here ++ ": unsupported executable pattern: " ++ pretty pattern' | not (patternForm form)]
            in (problems, []) `plus` descend here pattern'
        | Just (_ :: ForeignDecl GhcRn) <- cast value = bad "foreign declaration"
        | Just (_ :: RuleDecl GhcRn) <- cast value = bad "rewrite rule"
        | Just (_ :: AnnDecl GhcRn) <- cast value = bad "annotation splice"
        | Just name <- cast value :: Maybe Name, isVarName name || isDataConName name || isFieldName name =
            checkName location name
        | otherwise = descend location value
      where
        bad form = ([pretty location ++ ": unsupported executable form: " ++ form], [])
    descend location value = foldr plus ([], []) (children (walk location) value)
    strictBinding FunBind{fun_matches = matches} = strictNullaryRhs matches
    strictBinding _ = False
    nested location (Declaration binding) = walk location binding `plus`
        ([], [PatternAt (getLocA pat) | PatBind{pat_lhs = pat} <- [binding], Nothing <- [bindingName binding]])
    nested location (Component pat rhs) = walk location pat `plus` walk location rhs `plus`
        ([], [ComponentAt (getLocA pat) (getLocA rhs)])
    plus (a, b) (c, d) = (a ++ c, b ++ d)
    valueUse here name
        | permitted name, not (isQuickCheckBinding qc name), valueAt qcUses here name =
            ([pretty here ++ ": QuickCheck value used outside a generator or property: " ++ describeName name], [])
        | otherwise = ([], [])
    -- Matching or assigning a labelled field is not applying its selector.
    -- The constructor, field ownership, and nested pattern/expression still count.
    recordField :: Data rhs => SrcSpan -> HsRecField GhcRn rhs -> ([String], [SourceSite])
    recordField location field = checkOperation (getLocA label) (foExt (unLoc label)) `plus` walk location (hfbRHS field)
      where label = hfbLHS field
    checkName location name | isQuickCheckBinding qc name =
        ([pretty location ++ ": QuickCheck binding used outside a generator or property: " ++ describeName name], [])
    checkName location name = checkOperation location name
    checkOperation location name = if permitted name then ([], []) else
        ([pretty location ++ ": unapproved " ++ (if runtimeOwns rt name then "runtime" else "external") ++
          " operation " ++ describeName name ++ " is forbidden in generated code"], [])
    permitted name | name `elem` restrictedViews support = False
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

-- A selected constructor identifies the right parent, not merely a candidate
-- with the same field spelling. Parent labels are ordered like update fields.
ambiguousUpdateReferences :: Data a => a -> [(SrcSpan, Name)]
ambiguousUpdateReferences checked =
    [ (getLocA label, greName field)
    | ExpandedThingTc (OrigExpr RecordUpd { rupd_flds = RegularRecUpdFields parents fields }) expanded <- collectAst checked
    , let selected = map (conLikeName . fst) (updateAlternatives expanded)
    , parent <- toList parents
    , length fields == length (rnRecUpdLabels parent)
    , any ((`elem` selected) . conLikeName_Name) (nonDetEltsUniqSet (rnRecUpdCons parent))
    , (binding, field) <- zip fields (toList (rnRecUpdLabels parent))
    , Ambiguous _ label <- [unLoc (hfbLHS (unLoc binding))]
    ]

-- These are the pinned compiler's finite section templates, not general
-- permission to discard an expansion. The operator is the same resolved Name;
-- the operand retains its checked source site. Right sections use the exact
-- wired-in compiler identity, never a same-spelled home or imported helper.
sectionExpansion :: XXExprGhcRn -> Bool
sectionExpansion (ExpandedThingRn (OrigExpr (SectionL _ operand operator)) (HsApp _ actualOperator actualOperand)) =
    sameOperator operator actualOperator && getLocA operand == getLocA actualOperand
sectionExpansion (ExpandedThingRn (OrigExpr (SectionR _ operator operand))
        (HsApp _ (L _ (HsApp _ (L _ (HsVar _ helper)) actualOperator)) actualOperand)) =
    unLoc helper == rightSectionName && sameOperator operator actualOperator && getLocA operand == getLocA actualOperand
sectionExpansion _ = False

sameOperator :: LHsExpr GhcRn -> LHsExpr GhcRn -> Bool
sameOperator expected actual = case (variable expected, variable actual) of
    (Just a, Just b) -> a == b
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
