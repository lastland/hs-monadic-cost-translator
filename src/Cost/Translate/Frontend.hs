{-# LANGUAGE LambdaCase #-}
module Cost.Translate.Frontend (generate) where

import GHC
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import GHC.Data.Bag (bagToList)
import GHC.Paths (libdir)
import GHC.Driver.Session (xopt)
import qualified GHC.LanguageExtensions.Type as Extension
import GHC.Types.Name (nameModule_maybe, getOccString)
import GHC.Types.Unique (getUnique)
import GHC.Types.Basic (RecFlag(..), Boxity(..))
import GHC.Types.Var (varName, isId)
import GHC.Core.Type (splitForAllTyVars, splitFunTys, isPredTy, splitTyConApp_maybe)
import GHC.Core.TyCo.Rep (scaledThing)
import Cost.Translate.Datatypes (renderType, emitDatatypeSections, constructors, selectors, checkConversionExports)
import Cost.Translate.Names (generatedName)
import Cost.Translate.Pretty
    (Section(..), prettyModule, basicExtensions, polymorphicExtensions, reflectionExtensions, requiredExtensions)
import qualified Cost.Translate.Output as O
import Cost.Translate.Kinds (renderBinder)
import Cost.Translate.Sharing (constrainedVariables, sharingPaths, sharingSupport)
import Cost.Translate.Callable (callArity, emitCallable, isComposition, spine)
import Cost.Translate.Diagnostic (Diagnostic(..))
import Cost.Translate.Typed (hasFreeVariables, hasFreshAnnotationVariables)
import qualified Cost.Translate.Typed as Typed
import Data.Bifunctor (first)
import Cost.Translate.Foldable (foldableSupport)
import Cost.Translate.Validation (validateComparisons, scalarSupport)
import GHC.Unit.Module.ModGuts (mg_binds)
import GHC.Utils.Outputable (ppr, showSDocUnsafe, showSDocOneLine, defaultSDocContext, SDocContext(sdocSuppressUniques))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (isPrefixOf)
import Data.Data (Data, cast, gmapQ)
import Data.Maybe (maybeToList, isJust)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SourceText (SourceText(NoSourceText))
import GHC.Data.FastString (unpackFS)
import System.FilePath (takeDirectory)

-- Work from the renamed syntax, after GHC has checked and inferred the source.
-- In particular, no simplifier or optimiser runs before instrumentation.
generate :: FilePath -> String -> IO (Either Diagnostic String, String)
generate file targetModule = runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    _ <- setSessionDynFlags flags { backend = noBackend, ghcLink = NoLink,
                                   importPaths = [takeDirectory file] }
    target <- guessTarget file Nothing Nothing
    setTargets [target]
    graph <- depanal [] False
    case mgModSummaries graph of
        [summary] -> do
            checked <- parseModule summary >>= typecheckModule
            desugared <- desugarModule checked
            session <- getSession
            hints <- case tm_renamed_source checked of
                Just (group,_,_,_,_) -> liftIO (Typed.expressionTypes session checked (expressions group) (collect (tm_typechecked_source checked)))
                Nothing -> pure Map.empty
            pure $ case tm_renamed_source checked of
                Just (group, imports, _, _, _) ->
                    let types = Map.fromList [(varName i, idType i) | AnId i <- modInfoTyThings (tm_checked_module_info checked)] `Map.union`
                            Map.fromList [(varName i, idType i) | i <- collect (tm_typechecked_source checked) :: [Id], isId i]
                        datatypes = [t | ATyCon t <- modInfoTyThings (tm_checked_module_info checked)]
                        literalTypes = Map.fromList
                            [(showSDocUnsafe (ppr (getLocA expression)), name) | expression@(L _ (HsOverLit _ literal)) <- collect (tm_typechecked_source checked) :: [LHsExpr GhcTc],
                             Just (tc,[]) <- [splitTyConApp_maybe (overLitType literal)],
                             let name = getOccString tc, name `elem` ["Int","Integer","Float","Double"]]
                        expressionTypes = Map.fromList
                            [(location, (hasFreeVariables ty, rendered)) |
                             (location,ty) <- Map.toList hints, Right rendered <- [renderType datatypes ty]]
                        (result, logs) = emitModule targetModule (moduleNameString (moduleName (ms_mod summary)))
                             (xopt Extension.MagicHash (ms_hspp_opts summary)) datatypes types literalTypes expressionTypes group
                        validation = do
                            when (any (`xopt` ms_hspp_opts summary) [Extension.Strict, Extension.StrictData, Extension.RebindableSyntax])
                                (Left (file ++ ":1: Strict, StrictData and RebindableSyntax are unsupported"))
                            mapM_ checkImport imports
                            mapM_ checkDeclaration (hsmodDecls (unLoc (parsedSource checked)))
                            checkConversionExports (modInfoExports (tm_checked_module_info checked)) datatypes
                            validateComparisons (mg_binds (coreModule desugared))
                    in (first UnsupportedSource (validation >> result) >>= \(ast, sections, extensions) ->
                        prettyModule flags extensions ast sections, logs)
                Nothing -> (Left (UnsupportedSource (file ++ ": GHC did not retain renamed source")), "")
        _ -> pure (Left (UnsupportedSource (file ++ ": expected one source module")), "")

checkImport :: LImportDecl GhcRn -> Either String ()
checkImport located = unless (moduleNameString (unLoc (ideclName (unLoc located))) `elem` ["Prelude", "Memorist.Example.NewtonRaphson"]) $
    Left (showSDocUnsafe (ppr (getLocA located)) ++ ": unsupported import")

-- Fail rather than silently omitting any as-yet unimplemented declaration.
-- These are implementation gaps, not additional restrictions on the PRD.
checkDeclaration :: LHsDecl GhcPs -> Either String ()
checkDeclaration located = case unLoc located of
    ValD {} -> Right ()
    SigD {} -> Right ()
    TyClD _ DataDecl { tcdDataDefn = defn } | null (dd_derivs defn) -> Right ()
    TyClD _ DataDecl {} -> Left (showSDocUnsafe (ppr (getLocA located)) ++ ": source deriving clauses are unsupported")
    _ -> Left (showSDocUnsafe (ppr (getLocA located)) ++ ": unsupported declaration")

emitModule :: String -> String -> Bool -> [TyCon] -> Map.Map Name Type -> Map.Map String String -> Map.Map String (Bool, O.Ty) -> HsGroup GhcRn -> (Either String (HsModule GhcPs, [Section], [Extension.Extension]), String)
emitModule target source magicHash datatypes types literalTypes expressionTypes group =
    let binds = case hs_valds group of
            XValBindsLR (NValBinds groups _) -> concatMap (bagToList . snd) groups
            ValBinds _ bs _ -> bagToList bs
        env = extendEnv [(unLoc n, Function (generatedName "M" (unLoc n)) (arity ms))
                         | L _ (FunBind _ n ms) <- binds] constructorEnv
        allBinds = collect group :: [LHsBind GhcRn]
        logEnv = extendEnv [(unLoc n, Function (getOccString (unLoc n)) (arity ms)) | L _ (FunBind _ n ms) <- allBinds] constructorEnv
        logs = unlines ("GHC parsed, renamed and typechecked source; lazy A-normalization before optimization" :
                [showSDocUnsafe (ppr (getLocA b)) ++ ": " ++ identity (unLoc n)
                 ++ " arity " ++ show (arity ms) | b@(L _ (FunBind _ n ms)) <- allBinds] ++
                concatMap (logExpression logEnv) (expressions group))
        result = do
            (datatypeSupport, kindSupport, datatypeDeclarations) <- emitDatatypeSections datatypes
            fields <- mapM bindingField binds
            declarations <- if any (\case L _ (FunBind _ _ ms) -> arity ms == 0; _ -> False) binds
                then do
                    signatures <- sequence [(,) (unLoc n) <$> locatedAt (getLocA n) (translatedSignatureUsing
                        (unLoc n `Set.member` reflected) (arity ms == 0 && not (null (constrainedVariables ty))) datatypes (arity ms) ty) |
                        L _ (FunBind _ n ms) <- binds, Just ty <- [Map.lookup (unLoc n) types]]
                    initializer <- withTypedValBinds (Map.fromList signatures) constructorEnv (hs_valds group) $ \scope -> do
                        values <- mapM (bindingValue scope) binds
                        pure (O.ret (O.call "Bindings" values))
                    pure [O.recordD "Bindings" fields,
                          O.sig "initialize" (O.tc "M.MemoristM" [O.t "Bindings"]), O.val "initialize" [] initializer]
                else concat <$> sequence [do code <- emitBind env binding; pure [O.sig name ty,code] | (binding,(name,ty)) <- zip binds fields]
            let hasDatatypes = not (null datatypes)
                hasReflection = not (Set.null reflected)
                hasPolymorphicBindings = not (all (null . fst . splitForAllTyVars) (Map.elems types))
                extensions = addMagicHash $ if hasDatatypes
                    then requiredExtensions False
                    else if hasReflection then reflectionExtensions
                    else if hasPolymorphicBindings then polymorphicExtensions
                    else basicExtensions
                addMagicHash enabled = [Extension.MagicHash | magicHash] ++ enabled
                common = [ Section True "sharing and scalar support" (sharingSupport hasReflection ++ scalarSupport)
                         , Section True "foldable support" foldableSupport
                         , Section True "datatype conversion and demand support" datatypeSupport
                         ]
                specific = [ Section False ("source datatypes and conversions from " ++ source) (kindSupport ++ datatypeDeclarations)
                           , Section False ("translated bindings from " ++ source) declarations
                           ]
                sections = common ++ specific
                usedQualifiers = Set.fromList
                    [moduleNameString qualifier | Qual qualifier _ <- collect (concatMap sectionDeclarations sections) :: [RdrName]]
                usedUnqualified = Set.fromList
                    [occNameString occurrence | Unqual occurrence <- collect (concatMap sectionDeclarations sections) :: [RdrName]]
                hidden = Set.toList (Set.filter isPreludeExport (Set.fromList
                    ([generatedName "M" (unLoc n) | L _ (FunBind _ n _) <- binds] ++ map snd (selectors datatypes))))
                qualifiedImport name alias = [O.importD name (Just alias) | alias `Set.member` usedQualifiers]
                imports = [if null hidden then O.importD "Prelude" Nothing else O.hideImport "Prelude" hidden] ++
                    [O.importD "Data.Kind" Nothing | "Type" `Set.member` usedUnqualified] ++
                    [O.importD "Data.Proxy" Nothing | "Proxy" `Set.member` usedUnqualified] ++
                    concat [ qualifiedImport "Memorist.Memorist" "M", qualifiedImport "GHC.TypeLits" "TypeLits"
                           , qualifiedImport "Type.Reflection" "Reflection", qualifiedImport "Data.List.NonEmpty" "NE"
                           , qualifiedImport "Data.Char" "Char", qualifiedImport source "Source"
                           , qualifiedImport "Test.QuickCheck" "QC", qualifiedImport "Memorist.Example.List" "ML"
                           , qualifiedImport "Memorist.Example.ListInf" "MI"
                           , qualifiedImport "Memorist.Example.NewtonRaphson" "MN"
                           , qualifiedImport "Memorist.Data.List" "L" ]
            pure (O.moduleD target imports [], sections, extensions)
    in (result, logs)
  where
    isPreludeExport name = name `elem`
        ["mapM", "mapM_", "sequence", "sequence_", "foldr", "foldl", "filter", "zipWith", "concatMap"]
    constructorEnv = Env (Map.fromList ([(n, Constructor c a fs) | (n,c,a,fs) <- constructors datatypes] ++
                                  [(n, Function f 1) | (n,f) <- selectors datatypes])) literalTypes types expressionTypes datatypes reflected
                                  (Set.fromList ("M.PairT" : [generatedName "T" (getName c) | tc <- datatypes, [c] <- [tyConDataCons tc]]))
    sourceBinds = collect group :: [LHsBind GhcRn]
    reflected = closure (Set.fromList [unLoc n | L _ (FunBind _ n ms) <- sourceBinds, arity ms == 0,
        Just ty <- [Map.lookup (unLoc n) types], not (null (constrainedVariables ty))])
    closure known = let next = known `Set.union` Set.fromList
                            [unLoc n | b@(L _ (FunBind _ n _)) <- sourceBinds,
                             any (`Set.member` known) [unLoc v | L _ (HsVar _ v) <- expressions b]]
                    in if next == known then known else closure next
    bindingField (L _ (FunBind _ name matches)) = do
        ty <- maybe (Left "missing inferred binding type") Right (Map.lookup (unLoc name) types)
        signature <- either (Left . ((showSDocUnsafe (ppr (getLocA name)) ++ ": ") ++)) Right (translatedSignatureWith (unLoc name `Set.member` reflected) datatypes (arity matches) ty)
        pure (generatedName "M" (unLoc name), signature)
    bindingField b = bindingError b
    bindingValue scope (L _ (FunBind _ name _)) = case lookupEnv (unLoc name) scope of
        Just (Thunk n) -> Right n
        Just (Function n _) -> Right (O.v n)
        _ -> Left "missing initialized binding"
    bindingValue _ b = bindingError b

bindingError :: LHsBind GhcRn -> Either String a
bindingError located = Left (showSDocUnsafe (ppr (getLocA located)) ++ ": " ++ detail)
  where
    detail = case unLoc located of
        PatBind { pat_lhs = pat } -> "unsupported pattern binding " ++
            showSDocOneLine defaultSDocContext { sdocSuppressUniques = True } (ppr pat) ++ "; only named value bindings are supported"
        _ -> "unsupported binding form: " ++ showSDocUnsafe (ppr located)

-- Signature/demand diagnostics reach users through ADR-0002 locations.
locatedAt :: SrcSpan -> Either String a -> Either String a
locatedAt srcSpan = either (Left . ((showSDocUnsafe (ppr srcSpan) ++ ": ") ++)) Right

collect :: (Data a, Data b) => b -> [a]
collect x = maybeToList (cast x) ++ concat (gmapQ collect x)

-- Descend into application arguments and the head, never intermediate HsApp
-- nodes: f x y is one saturated call, not also a partial application of f x.
expressions :: Data a => a -> [LHsExpr GhcRn]
expressions value = case cast value of
    Just expr -> expr : case unLoc expr of
        HsApp {} -> let (fun,args) = spine expr in concatMap expressions (fun:args)
        _ -> concat (gmapQ expressions (unLoc expr))
    Nothing -> concat (gmapQ expressions value)

identity :: Name -> String
identity n = maybe "local" (moduleNameString . moduleName) (nameModule_maybe n) ++ "." ++ getOccString n

logExpression :: Env -> LHsExpr GhcRn -> [String]
logExpression env expr = case unLoc expr of
    HsApp {} -> case spine expr of
        (L _ (HsVar _ name), _) | isComposition (unLoc name) ->
            [location ++ identity (unLoc name) ++ ": prefix composition adapter (zero ticks); " ++
             either id (("remaining arity " ++) . show) (remainingArity env expr)]
        (L _ (HsVar _ name), args) -> case resolve env (unLoc name) of
            Just (Function _ count) -> [location ++ identity (unLoc name) ++ ": supplied " ++ show (length args) ++ " / arity " ++ show count ++ if length args < count then "; partial adapter (zero ticks)" else "; saturated call"]
            Just (Constructor _ count _) -> [location ++ identity (unLoc name) ++ ": constructor fields " ++ show (length args) ++ " / " ++ show count]
            Just (Adapter _ count) -> [location ++ identity (unLoc name) ++ ": supplied " ++ show (length args) ++ " / arity " ++ show count]
            _ -> [location ++ identity (unLoc name) ++ ": calling convention unavailable"]
        _ -> [location ++ "computed/composed application: validate remaining arity"]
    OpApp _ _ op _ | HsVar _ name <- unLoc op, builtin (unLoc name) "GHC.Internal.Base" "." -> [location ++ identity (unLoc name) ++ ": composition adapter (zero ticks)"]
    _ -> []
  where location = showSDocUnsafe (ppr (getLocA expr)) ++ ": "

translatedSignatureWith :: Bool -> [TyCon] -> Int -> Type -> Either String O.Ty
translatedSignatureWith reflected = translatedSignatureUsing reflected False

translatedSignatureUsing :: Bool -> Bool -> [TyCon] -> Int -> Type -> Either String O.Ty
translatedSignatureUsing reflected factory datatypes count ty = do
    let (vars, body) = splitForAllTyVars ty
        (scaledArgs, result) = splitFunTys body
        (constraints, args) = span isPredTy (map scaledThing scaledArgs)
    quantified <- mapM (renderBinder datatypes) vars
    sourceConstraints <- concat <$> mapM translatedConstraint constraints
    let renderedConstraints = sourceConstraints ++ [O.tc "Reflection.Typeable" [O.t (getOccString v)] | reflected, v <- constrainedVariables ty]
    unless (length args == count) (Left "function-valued result has no validated calling convention")
    argTypes <- mapM (renderType datatypes) args
    resultType <- renderType datatypes result
    pure $ O.forallT quantified $ O.qualified renderedConstraints $
        if count == 0 then O.arrows [O.t "()" | factory] (O.tc "M.Th" [resultType])
        else O.arrows (map (\a -> O.tc "M.Th" [a]) argTypes) (O.tc "M.MemoristM" [resultType])

translatedConstraint :: Type -> Either String [O.Ty]
translatedConstraint ty = case splitTyConApp_maybe ty of
    Just (tc,[arg]) | builtin (getName tc) "GHC.Internal.Data.Foldable" "Foldable" ->
        (\a -> [O.tc "FoldableT" [a]]) <$> renderType [] arg
    Just (tc,args) | getOccString tc `elem` ["Num","Eq","Ord","Fractional","Floating","Integral","Real","RealFrac","RealFloat","Enum","Bounded"] -> do
        rendered <- mapM (renderType []) args
        pure (O.tc (getOccString tc) rendered : [O.tc "Scalar" [a] | getOccString tc `elem` ["Eq","Ord"], a <- rendered])
    _ -> Left ("unsupported type constraint: " ++ showSDocUnsafe (ppr ty))

data Binding = Thunk O.Expr | Function String Int | Adapter O.Expr Int | Constructor String Int [Name]
data Env = Env
    { envBindings :: Map.Map Name Binding
    , inferredLiterals :: Map.Map String String
    , inferredBindings :: Map.Map Name Type
    , inferredExpressions :: Map.Map String (Bool, O.Ty)
    , sourceDatatypes :: [TyCon]
    , reflectiveBindings :: Set.Set Name
    , singleConstructors :: Set.Set String
    }

lookupEnv :: Name -> Env -> Maybe Binding
lookupEnv name = Map.lookup name . envBindings

insertEnv :: Name -> Binding -> Env -> Env
insertEnv name binding env = env { envBindings = Map.insert name binding (envBindings env) }

extendEnv :: [(Name, Binding)] -> Env -> Env
extendEnv additions env = env { envBindings = Map.union (Map.fromList additions) (envBindings env) }

fieldName :: FieldOcc GhcRn -> Name
fieldName = foExt

freshThunk :: String -> Env -> String
freshThunk prefix env = choose (0 :: Int)
  where
    used = [occNameString (rdrNameOcc n) | Thunk e <- Map.elems (envBindings env), n <- collect e :: [RdrName]]
    choose i = let n = prefix ++ show i in
        if any (n `isPrefixOf`) used then choose (i+1) else n

arity :: MatchGroup GhcRn (LHsExpr GhcRn) -> Int
arity matches = case unLoc (mg_alts matches) of
    L _ m : _ -> length (m_pats m)
    [] -> 0

localName :: Name -> String
localName name = "v" ++ show (getUnique name) ++ "_"

emitBind :: Env -> LHsBind GhcRn -> Either String O.Decl
emitBind outer binding = case unLoc binding of
    FunBind _ name matches -> do
        unless (arity matches > 0) $
            Left (showSDocUnsafe (ppr (getLocA binding)) ++ ": top-level shared value initialization not implemented yet")
        let params = [localName (unLoc name) ++ "arg" ++ show i | i <- [0 .. arity matches - 1]]
        body <- emitMatches outer params (unLoc (mg_alts matches))
        fname <- case lookupEnv (unLoc name) outer of
            Just (Function n _) -> Right n
            _ -> Left "expected function binding"
        pure (O.val fname (map O.p params) (O.doE [O.thenS tick] body))
    _ -> bindingError binding

-- A failure continuation per equation preserves the order even when variable
-- and constructor patterns occur in the same column. No column regrouping.
emitMatches :: Env -> [String] -> [LMatch GhcRn (LHsExpr GhcRn)] -> Either String O.Expr
emitMatches _ _ [] = Right (failureExpr "source pattern match failure")
emitMatches env args (located@(L _ match) : rest) = do
    fallback <- emitMatches env args rest
    let next = actionName "nextEquation" (getLocA located)
        failure = if null rest then fallback else O.v next
    current <- matchPatterns env (zip args (m_pats match))
        (\scope -> withLocals scope (grhssLocalBinds (m_grhss match)) $ \scope' ->
            emitGuards scope' (grhssGRHSs (m_grhss match)) failure) failure
    pure (if null rest then current else shareAction next fallback current)

emitGuards :: Env -> [LGRHS GhcRn (LHsExpr GhcRn)] -> O.Expr -> Either String O.Expr
emitGuards _ [] fallback = Right fallback
emitGuards env [L _ (GRHS _ [] body)] _ = emitExpr env body
emitGuards env (located@(L _ (GRHS _ guards body)) : rest) fallback = do
    next <- emitGuards env rest fallback
    let name = actionName "nextGuard" (getLocA located)
    current <- go env guards (O.v name)
    pure (shareAction name next current)
  where
    go :: Env -> [GuardLStmt GhcRn] -> O.Expr -> Either String O.Expr
    go scope [] _ = emitExpr scope body
    go scope (L _ (BodyStmt _ expr _ _) : gs) next = do
        condition <- emitExpr scope expr
        remaining <- go scope gs next
        pure (O.doE [O.bind "guardResult" condition] (O.ifE (O.v "guardResult") remaining next))
    go scope (L _ (BindStmt _ pat expr) : gs) next = do
        action <- emitExpr scope expr
        let n = freshThunk "guardThunk" scope
        remaining <- matchPatterns scope [(n, pat)] (\scope' -> go scope' gs next) next
        pure (O.doE [O.bind n (O.call "M.lazy" [action])] remaining)
    go scope (L _ (LetStmt _ locals) : gs) next = withLocals scope locals (\scope' -> go scope' gs next)
    go _ _ _ = Left "unsupported guard statement"

-- Bind a failure continuation as an action, not a thunk. Only the selected
-- branch executes it; the binding introduces neither allocation nor ticks.
shareAction :: String -> O.Expr -> O.Expr -> O.Expr
shareAction name action = O.letE [O.val name [] action]

failureExpr :: String -> O.Expr
failureExpr message = O.call "error" [O.string message]

tick :: O.Expr
tick = O.ann (O.v "M.tick") (O.tc "M.MemoristM" [O.t "()"])

actionName :: String -> SrcSpan -> String
actionName prefix (RealSrcSpan location _) = prefix ++ concatMap (("_" ++) . show)
    [srcSpanStartLine location, srcSpanStartCol location, srcSpanEndLine location, srcSpanEndCol location]
actionName prefix _ = prefix ++ "_generated"

matchPatterns :: Env -> [(String, LPat GhcRn)] -> (Env -> Either String O.Expr) -> O.Expr -> Either String O.Expr
matchPatterns env [] success _ = success env
matchPatterns env ((thunk, located) : rest) success failure = case unLoc located of
    VarPat _ name -> continue (insertEnv (unLoc name) (Thunk (O.v thunk)) env)
    WildPat _ -> continue env
    ParPat _ pat -> matchPatterns env ((thunk, pat) : rest) success failure
    AsPat _ name pat -> matchPatterns (insertEnv (unLoc name) (Thunk (O.v thunk)) env) ((thunk, pat) : rest) success failure
    TuplePat _ [a,b] Boxed -> constructor "M.PairT" [a,b]
    ListPat _ [] -> constructor "L.NilT" []
    ListPat ext (p:ps) -> constructor "L.ConsT" [p, L (getLoc located) (ListPat ext ps)]
    NPat _ literal negative _ -> do
        body <- continue env
        let positive = O.overLit (ol_val (unLoc literal))
            value = if isJust negative then O.call "negate" [positive] else positive
        pure (literalCase value body)
    LitPat _ (HsChar _ c) -> continue env >>= \body -> pure (literalCase (O.char c) body)
    -- A string literal pattern is its sequence of character literals: forcing
    -- proceeds cell by cell and stops at the first mismatch, exactly like the
    -- source comparison, instead of comparing thunk representations.
    LitPat _ (HsString _ text) ->
        matchPatterns env ((thunk, noLocA (ListPat noExtField
            (map (noLocA . LitPat noExtField . HsChar NoSourceText) (unpackFS text)))) : rest) success failure
    ConPat _ name details -> do
        con <- case () of
            _ | Just (Constructor c _ _) <- lookupEnv (unLoc name) env -> Right c
            _ | builtin (unLoc name) "GHC.Types" ":" -> Right "L.ConsT"
              | builtin (unLoc name) "GHC.Types" "[]" -> Right "L.NilT"
              | builtin (unLoc name) "GHC.Types" "True" -> Right "True"
              | builtin (unLoc name) "GHC.Types" "False" -> Right "False"
              | Just (Constructor c _ _) <- resolve env (unLoc name) -> Right c
              | otherwise -> Left (showSDocUnsafe (ppr (unLoc name)) ++ ": constructor pattern not implemented yet")
        case details of
            PrefixCon _ pats -> constructor con pats
            InfixCon a b -> constructor con [a,b]
            RecCon fields | Just (Constructor _ count labels) <- lookupEnv (unLoc name) env -> do
                let slots = fieldSlots count
                -- Extract in representation order, but perform each supplied
                -- subpattern in source order. Reordering these tests can force
                -- a bottom in a field that a preceding mismatch would skip.
                ordered <- mapM (\f -> do
                    let field = unLoc f
                    slot <- maybe (Left (showSDocUnsafe (ppr (getLocA located)) ++ ": unknown record pattern field")) Right
                        (lookup (fieldName (unLoc (hfbLHS field))) (zip labels slots))
                    pure (slot, hfbRHS field)) (rec_flds fields)
                body <- matchPatterns env (ordered ++ rest) success failure
                let used = [slot | (slot, pat) <- ordered, not (wildcard pat)]
                pure (constructorCase con [if slot `elem` used then slot else "_" | slot <- slots] body)
            _ -> Left "unsupported record pattern"
    _ -> Left (showSDocUnsafe (ppr (getLocA located)) ++ ": pattern not implemented yet")
  where
    continue scope = matchPatterns scope rest success failure
    literalCase value body = O.doE [O.bind "matched" (O.call "M.force" [O.v thunk])]
        (O.ifE (O.op "==" (O.v "matched") value) body failure)
    constructor con pats = do
        let fields = zipWith (\field pat -> if wildcard pat then "_" else field) (fieldSlots (length pats)) pats
        body <- matchPatterns env (zip fields pats ++ rest) success failure
        pure (constructorCase con fields body)
    wildcard pat = case unLoc pat of
        WildPat _ -> True
        ParPat _ nested -> wildcard nested
        _ -> False
    fieldSlots count = [thunk ++ "f" ++ show i | i <- [0 :: Int .. count-1]]
    constructorCase con fields body = O.doE [O.bind "matched" (O.call "M.force" [O.v thunk])] $
        O.caseE (O.v "matched") ((O.conP con (map O.p fields), body) : [(O.p "_",failure) | con `Set.notMember` singleConstructors env])

emitLocals :: Env -> HsLocalBinds GhcRn -> LHsExpr GhcRn -> Either String O.Expr
emitLocals env locals body = withLocals env locals (`emitExpr` body)

withLocals :: Env -> HsLocalBinds GhcRn -> (Env -> Either String O.Expr) -> Either String O.Expr
withLocals env locals finish = case locals of
    EmptyLocalBinds _ -> finish env
    HsValBinds _ binds -> withValBinds env binds finish
    _ -> Left "local binding form not implemented yet"

withValBinds :: Env -> HsValBinds GhcRn -> (Env -> Either String O.Expr) -> Either String O.Expr
withValBinds env binds finish = do
    signatures <- case binds of
        XValBindsLR (NValBinds groups sigs) -> do
            let explicit = Set.fromList [unLoc n | L _ (TypeSig _ names _) <- sigs, n <- names]
                sigSpans = Map.fromList [(unLoc n, locA loc) | L loc (TypeSig _ names _) <- sigs, n <- names]
            sequence [(,) (unLoc name) <$> locatedAt (Map.findWithDefault (getLocA name) (unLoc name) sigSpans)
                (translatedSignatureUsing
                (unLoc name `Set.member` reflectiveBindings env)
                (arity matches == 0 && not (null (constrainedVariables ty))) (sourceDatatypes env) (arity matches) ty) |
                L _ (FunBind _ name matches) <- concatMap (bagToList . snd) groups,
                unLoc name `Set.member` explicit, Just ty <- [Map.lookup (unLoc name) (inferredBindings env)]]
        _ -> pure []
    withTypedValBinds (Map.fromList signatures) env binds finish

withTypedValBinds :: Map.Map Name O.Ty -> Env -> HsValBinds GhcRn -> (Env -> Either String O.Expr) -> Either String O.Expr
withTypedValBinds signatures env binds finish = case binds of
    XValBindsLR (NValBinds groups _) -> groupsGo env groups
    _ -> Left "unrenamed local bindings"
  where
    groupsGo scope [] = finish scope
    groupsGo scope ((recursive, bs):rest) = do
        let bindings = bagToList bs
            functions = [(unLoc n, Function (localName (unLoc n)) (arity ms)) | L _ (FunBind _ n ms) <- bindings, arity ms > 0]
        mapM_ (\b -> case unLoc b of { FunBind {} -> Right (); _ -> bindingError b }) bindings
        case recursive of
            Recursive -> do
                let location = case bindings of { b:_ -> showSDocUnsafe (ppr (getLocA b)); [] -> "empty binding group" }
                unless (length functions == length bindings) (Left (location ++ ": recursive value bindings are unsupported"))
                let scope' = extendEnv functions scope
                declarations <- concat <$> mapM (typedBind scope') bindings
                continuation <- groupsGo scope' rest
                pure (O.letE declarations continuation)
            NonRecursive -> go scope bindings (`groupsGo` rest)
    go scope [] done = done scope
    go scope (binding@(L _ (FunBind _ name matches)) : rest) done = case unLoc (mg_alts matches) of
        [L _ match] | null (m_pats match) -> do
                mapM_ (\(L _ (GRHS _ _ rhs)) -> case remainingArity scope rhs of
                    Right n | n > 0 -> Left (showSDocUnsafe (ppr (getLocA rhs)) ++ ": function-valued alias has no validated calling convention")
                    _ -> Right ()) (grhssGRHSs (m_grhss match))
                action <- withLocals scope (grhssLocalBinds (m_grhss match)) $ \scope' -> emitGuards scope' (grhssGRHSs (m_grhss match)) (failureExpr "source guard failure")
                let n = localName (unLoc name)
                paths <- locatedAt (getLocA name)
                    (maybe (Right Nothing) sharingPaths (Map.lookup (unLoc name) (inferredBindings scope)))
                let reference = if isJust paths then O.call n [O.tuple []] else O.v n
                continuation <- go (insertEnv (unLoc name) (Thunk reference) scope) rest done
                if any ((== n) . occNameString . rdrNameOcc) (collect continuation :: [RdrName])
                    then do
                        let signature = maybeToList (O.sig n <$> Map.lookup (unLoc name) signatures)
                            allocate = maybe (O.v "sharedAt") (\path -> O.call "sharedAtBy" [O.ann (O.v "Proxy") (O.tc "Proxy" [path])]) paths
                            parameters = [O.conP "()" [] | isJust paths]
                            declaration = O.val n parameters (O.app allocate [O.call "M.lazy" [action],O.v (n ++ "anchor")])
                        pure (O.doE [O.bind (n ++ "anchor") (O.call "M.lazy" [O.ret (O.tuple [])])] (O.letE (signature ++ [declaration]) continuation))
                    else pure continuation
        _ -> do
            let scope' = insertEnv (unLoc name) (Function (localName (unLoc name)) (arity matches)) scope
            declaration <- typedBind scope' binding
            continuation <- go scope' rest done
            pure (O.letE declaration continuation)
    go _ (b:_) _ = bindingError b
    typedBind scope b@(L _ (FunBind _ name _)) = do
        declaration <- emitBind scope b
        pure (maybeToList (O.sig (localName (unLoc name)) <$> Map.lookup (unLoc name) signatures) ++ [declaration])
    typedBind _ b = bindingError b

emitExpr :: Env -> LHsExpr GhcRn -> Either String O.Expr
emitExpr env located = case unLoc located of
    HsOverLit _ literal -> let value = O.overLit (ol_val literal) in Right (O.ret
        (maybe value (O.ann value . O.t) (Map.lookup (showSDocUnsafe (ppr (getLocA located))) (inferredLiterals env))))
    HsLit _ (HsChar _ c) -> Right (O.ret (O.char c))
    NegApp _ expression _ -> do
        action <- emitExpr env expression
        pure (O.doE [O.bind "operand" action] (O.ret (O.call "negate" [O.v "operand"])))
    HsVar _ name -> annotate False <$> case resolve env (unLoc name) of
        Just (Thunk thunk) -> Right (O.call "M.force" [thunk])
        Just (Function fn _) -> Right (O.ret (O.v fn))
        Just (Adapter fn _) -> Right (O.ret fn)
        Just (Constructor con 0 _) -> Right (O.ret (O.v con))
        Just Constructor {} -> unsupported
        Nothing | builtin (unLoc name) "GHC.Types" "True" -> Right (O.ret (O.v "True"))
        Nothing | builtin (unLoc name) "GHC.Internal.Base" "otherwise" -> Right (O.ret (O.v "True"))
        Nothing | builtin (unLoc name) "GHC.Types" "False" -> Right (O.ret (O.v "False"))
        Nothing | builtin (unLoc name) "GHC.Types" "[]" -> Right (O.ret (O.v "L.NilT"))
        Nothing | getOccString (unLoc name) == "undefined",
                  Just modl <- nameModule_maybe (unLoc name),
                  moduleNameString (moduleName modl) == "GHC.Internal.Err" ->
                    Right (O.ret (failureExpr "source undefined"))
        Nothing -> Left (showSDocUnsafe (ppr (getLocA located)) ++ ": unsupported resolved name " ++ showSDocUnsafe (ppr (unLoc name)) ++ " in " ++ maybe "local scope" (moduleNameString . moduleName) (nameModule_maybe (unLoc name)))
    HsPar _ expr -> emitExpr env expr
    ExprWithTySig _ expr signature -> annotate (not (hasFreshAnnotationVariables signature)) <$> emitExpr env expr
    HsIf _ condition yes no -> do
        cond <- emitExpr env condition
        yesAction <- emitExpr env yes
        noAction <- emitExpr env no
        pure (O.doE [O.bind "condition" cond] (O.ifE (O.v "condition") yesAction noAction))
    HsLet _ locals body -> emitLocals env locals body
    ExplicitTuple _ [Present _ a, Present _ b] Boxed -> construct "M.PairT" [a,b]
    RecordCon _ name fields -> case lookupEnv (unLoc name) env of
        Just (Constructor con count labels) -> do
            actions <- mapM (\label -> maybe (Right (O.ret (failureExpr "source missing record field"))) (emitExpr env) (lookup label [(fieldName (unLoc (hfbLHS (unLoc f))), hfbRHS (unLoc f)) | f <- rec_flds fields])) labels
            unless (length actions == count) (Left "record construction field layout mismatch")
            pure (lazyArguments argumentPrefix actions (returnConstructor con))
        _ -> unsupported
    HsRecSel _ field -> case resolve env (fieldName field) of
        Just (Function fn _) -> Right (O.ret (O.v fn))
        _ -> unsupported
    HsCase _ scrutinee matches -> do
        action <- emitExpr env scrutinee
        let n = freshThunk "caseThunk" env
        body <- emitMatches env [n] (unLoc (mg_alts matches))
        pure (O.doE [O.bind n (O.call "M.lazy" [action])] body)
    ExplicitList _ elements -> list (0 :: Int) elements
    HsApp {} -> let (fun, args) = spine located in case unLoc fun of
        HsVar _ name | isComposition (unLoc name) -> callable
        HsVar _ name | builtin (unLoc name) "GHC.Internal.Err" "error",
            [L _ (HsLit _ (HsString _ text))] <- args -> Right (O.ret (failureExpr (unpackFS text)))
        HsRecSel _ field | Just (fn,_) <- functionResolver env (fieldName field), length args == 1 -> apply fn args
        HsVar _ name -> case resolve env (unLoc name) of
            Just (Constructor con count _) | length args == count -> construct con args
            _ | Just (fn,count) <- functionResolver env (unLoc name), length args <= count -> do
                actions <- mapM (emitExpr env) args
                pure (lazyArguments argumentPrefix actions (\names -> let invocation = O.app fn names in
                    if length args == count then invocation else O.ret invocation))
            _ -> Left (showSDocUnsafe (ppr (getLocA located)) ++ ": unsupported call " ++ getOccString (unLoc name) ++ " from " ++ maybe "local scope" (moduleNameString . moduleName) (nameModule_maybe (unLoc name)))
        _ -> callable
    OpApp _ left operator right -> case unLoc operator of
        HsVar _ name | isComposition (unLoc name) -> callable
        HsVar _ name | builtin (unLoc name) "GHC.Types" ":" -> construct "L.ConsT" [left,right]
        HsVar _ name | Just (Constructor con 2 _) <- resolve env (unLoc name) -> construct con [left,right]
        HsVar _ name | Just (fn,2) <- functionResolver env (unLoc name) -> apply fn [left,right]
        HsVar _ name -> Left (showSDocUnsafe (ppr (getLocA located)) ++ ": unsupported operator " ++ getOccString (unLoc name) ++ " from " ++ maybe "local scope" (moduleNameString . moduleName) (nameModule_maybe (unLoc name)))
        _ -> unsupported
    _ -> unsupported
  where
    annotate allowFree code = case Map.lookup (showSDocUnsafe (ppr (getLocA located))) (inferredExpressions env) of
        Just (free,ty) | allowFree || not free -> O.ann code (O.tc "M.MemoristM" [ty])
        _ -> code
    callable = emitCallable (functionResolver env) (emitExpr env) located
    argumentPrefix = actionName "argument" (getLocA located)
    apply fn args = do
        actions <- mapM (emitExpr env) args
        pure (lazyArguments argumentPrefix actions (O.app fn))
    construct con args = do
        actions <- mapM (emitExpr env) args
        pure (lazyArguments argumentPrefix actions (returnConstructor con))
    returnConstructor con names = O.ret (O.call con names)
    list _ [] = Right (O.ret (O.v "L.NilT"))
    list depth (x:xs) = do
        h <- emitExpr env x
        t <- list (depth + 1) xs
        pure (lazyArguments (argumentPrefix ++ "_list" ++ show depth) [h,t] (returnConstructor "L.ConsT"))
    unsupported = Left (showSDocUnsafe (ppr (getLocA located)) ++ ": unsupported expression " ++ showSDocUnsafe (ppr located))

lazyArguments :: String -> [O.Expr] -> ([O.Expr] -> O.Expr) -> O.Expr
lazyArguments prefix actions finish = O.doE [O.bind name (O.call "M.lazy" [action]) | (name,action) <- zip names actions]
    (finish (map O.v names))
  where names = [prefix ++ "_" ++ show i | i <- [0 :: Int .. length actions - 1]]

builtin :: Name -> String -> String -> Bool
builtin name modName occurrence = getOccString name == occurrence &&
    maybe False ((== modName) . moduleNameString . moduleName) (nameModule_maybe name)

resolve :: Env -> Name -> Maybe Binding
resolve env name = case lookupEnv name env of
    Just binding -> Just binding
    Nothing
        | builtin name "GHC.Internal.Maybe" "Just" -> Just (Constructor "M.JustT" 1 [])
        | builtin name "GHC.Internal.Maybe" "Nothing" -> Just (Constructor "M.NothingT" 0 [])
        | builtin name "GHC.Internal.List" "head" -> Just (Function "ML.headM" 1)
        | builtin name "GHC.Internal.List" "tail" -> Just (Function "ML.tailM" 1)
        | builtin name "GHC.Internal.List" "filter" -> Just (Function "ML.filterM" 2)
        | builtin name "GHC.Internal.List" "scanl" -> Just (Function "MI.scanlM" 3)
        | builtin name "GHC.Internal.Base" "map" -> Just (Function "ML.mapMM" 2)
        | builtin name "GHC.Internal.Base" "foldr" -> Just (Function "ML.foldrM" 3)
        | builtin name "GHC.Internal.Data.Foldable" "foldr" -> Just (Function "foldrT" 3)
        | builtin name "GHC.Internal.Data.Foldable" "foldl" -> Just (Function "foldlT" 3)
        | builtin name "GHC.Internal.Data.Foldable" "concatMap" -> Just (Function "concatMapT" 2)
        | builtin name "GHC.Internal.List" "foldl" -> Just (Function "ML.foldlM" 3)
        | builtin name "GHC.Internal.List" "zipWith" -> Just (Function "ML.zipWithM" 3)
        | builtin name "GHC.Internal.List" "concatMap" -> Just (Function "ML.concatMapM" 2)
        | builtin name "Memorist.Example.NewtonRaphson" "repeatly" -> Just (Adapter
            (O.lam (map O.p ["tf","tx"]) (O.doE [O.bind "f" (O.call "M.force" [O.v "tf"])] (O.call "MN.repeatlyM" [O.v "f",O.v "tx"]))) 2)
        | Just operation <- primitive name -> Just operation
        | otherwise -> Nothing

primitive :: Name -> Maybe Binding
primitive name
    | any (\m -> builtin name m occurrence) modules = Just (adapter
        (O.doE [O.bind "a" (force "ta"), O.bind "b" (force "tb")] (O.ret (O.op occurrence (O.v "a") (O.v "b")))))
    | any (\m -> builtin name m "&&") ["GHC.Classes","GHC.Internal.Classes"] = Just (adapter
        (O.doE [O.bind "a" (force "ta")] (O.ifE (O.v "a") (force "tb") (O.ret (O.v "False")))))
    | any (\m -> builtin name m "||") ["GHC.Classes","GHC.Internal.Classes"] = Just (adapter
        (O.doE [O.bind "a" (force "ta")] (O.ifE (O.v "a") (O.ret (O.v "True")) (force "tb"))))
    | otherwise = Nothing
  where
    occurrence = getOccString name
    adapter body = Adapter (O.lam (map O.p ["ta","tb"]) body) 2
    force n = O.call "M.force" [O.v n]
    modules
        | occurrence `elem` ["+","-","*"] = ["GHC.Internal.Num"]
        | occurrence `elem` ["==","/=","<",">","<=",">="] = ["GHC.Classes","GHC.Internal.Classes"]
        | occurrence `elem` ["/","div","mod","quot","rem"] = ["GHC.Internal.Real"]
        | otherwise = []

remainingArity :: Env -> LHsExpr GhcRn -> Either String Int
remainingArity env = callArity (functionResolver env)

functionResolver :: Env -> Name -> Maybe (O.Expr, Int)
functionResolver env name = case resolve env name of
    Just (Function fn count) -> Just (O.v fn,count)
    Just (Adapter fn count) -> Just (fn,count)
    _ -> Nothing
