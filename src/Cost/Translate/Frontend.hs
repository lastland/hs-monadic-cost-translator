{-# LANGUAGE LambdaCase #-}
-- | The core translation pass: one GHC session turns one source module into
-- the text of one instrumented module.
--
-- Session shape: 'generate' configures @backend = noBackend@ and
-- @ghcLink = NoLink@, so no optimizer or linker runs before
-- instrumentation. The pipeline is parse, typecheck, desugar; translation
-- works on the /renamed/ syntax. The desugared Core is read in exactly two
-- places, and never emitted from: type evidence via
-- "Cost.Translate.Typed" ('Typed.expressionTypes') and the scalar
-- comparison gate via "Cost.Translate.Validation"
-- ('validateComparisons').
--
-- Whole-module discipline: any unsupported construct fails the entire
-- translation with a source-located message rather than being skipped;
-- imports are whitelisted to @Prelude@ and
-- @Memorist.Example.NewtonRaphson@; library replacements match by resolved
-- imported identity, not name spelling.
--
-- Assembly: 'emitModule' builds the common sections (sharing and scalar
-- support, foldable support, datatype conversion and demand support) and
-- the module-specific sections (source datatypes and conversions,
-- translated bindings). When top-level value bindings exist, the output
-- exports a @Bindings@ record and @initialize :: MemoristM Bindings@
-- instead of standalone exported bindings.
--
-- Invariants carried through emission:
--
-- * The 'NamePlan' pre-pass allocates every fresh generated identifier up
--   front, so emission never invents a name and collisions cannot occur.
-- * Each translated binding ticks once on entry.
-- * Pattern and guard failure continuations are let-bound actions, not
--   thunks ('shareAction'), so only the selected branch runs them and the
--   binding adds neither allocation nor ticks.
module Cost.Translate.Frontend (generate) where

import GHC
import Control.Monad (unless, when, zipWithM)
import Control.Monad.IO.Class (liftIO)
import GHC.Data.Bag (bagToList)
import GHC.Paths (libdir)
import GHC.Driver.Session (xopt)
import qualified GHC.LanguageExtensions.Type as Extension
import GHC.Types.Name (nameModule_maybe, getOccString)
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
import Data.List (elemIndex, isPrefixOf, nub)
import Data.Char (isAlpha, isAlphaNum)
import Data.Data (Data, cast, gmapQ)
import Data.Maybe (maybeToList, isJust)
import GHC.Types.Name.Occurrence (occNameString, isTvOcc)
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
    let binds = topLevelBinds
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
                else concat <$> sequence [do code <- emitBind (env { namePlan = planNames reservedNames constructorLayouts binding }) binding
                                             pure [O.sig name ty,code] |
                                          (binding,(name,ty)) <- zip binds fields]
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
                                   (planNames reservedNames constructorLayouts group)
    constructorLayouts = Map.fromList [(n,fields') | (n,_,_,fields') <- constructors datatypes]
    topLevelBinds = case hs_valds group of
        XValBindsLR (NValBinds groups _) -> concatMap (bagToList . snd) groups
        ValBinds _ bs _ -> bagToList bs
    reservedNames = Set.fromList ([generatedName "M" (unLoc n) | L _ (FunBind _ n _) <- topLevelBinds] ++
        map snd (selectors datatypes))
    sourceBinds = collect group :: [LHsBind GhcRn]
    -- Bindings whose sharing is dictionary-sensitive: zero-arity bindings
    -- with constrained inferred types, closed under use by other bindings.
    -- These alone gain Typeable evidence and sharedAtBy allocation.
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
    , namePlan :: NamePlan
    }

data NamePlan = NamePlan
    { plannedLocals :: Map.Map Name String
    , plannedParameters :: Map.Map (Name, Int) String
    , plannedArguments :: Map.Map String [String]
    , plannedActions :: Map.Map String String
    , plannedFields :: Map.Map String String
    , plannedMatches :: Map.Map String String
    , plannedAnchors :: Map.Map Name String
    }

lookupEnv :: Name -> Env -> Maybe Binding
lookupEnv name = Map.lookup name . envBindings

insertEnv :: Name -> Binding -> Env -> Env
insertEnv name binding env = env { envBindings = Map.insert name binding (envBindings env) }

extendEnv :: [(Name, Binding)] -> Env -> Env
extendEnv additions env = env { envBindings = Map.union (Map.fromList additions) (envBindings env) }

fieldName :: FieldOcc GhcRn -> Name
fieldName = foExt

arity :: MatchGroup GhcRn (LHsExpr GhcRn) -> Int
arity matches = case unLoc (mg_alts matches) of
    L _ m : _ -> length (m_pats m)
    [] -> 0

localName :: Env -> Name -> String
localName env name = Map.findWithDefault "local" name (plannedLocals (namePlan env))

parameterNames :: Env -> Name -> MatchGroup GhcRn (LHsExpr GhcRn) -> [String]
parameterNames env name matches =
    [maybe (planned i) (readableBinder env) (patternBinder pattern') |
     (i,pattern') <- zip [0..] (firstPatterns matches)]
  where planned i = Map.findWithDefault ("arg" ++ show i) (name,i) (plannedParameters (namePlan env))

patternBinder :: LPat GhcRn -> Maybe Name
patternBinder (L _ pattern') = case pattern' of
    VarPat _ name -> Just (unLoc name)
    AsPat _ name _ -> Just (unLoc name)
    ParPat _ nested -> patternBinder nested
    _ -> Nothing

readableBinder :: Env -> Name -> String
readableBinder env name = freshPrimed (occupiedNames env) base
  where
    occurrence = getOccString name
    base = if simpleIdentifier occurrence then occurrence else "local"

occupiedNames :: Env -> Set.Set String
occupiedNames env = fixedReservedNames `Set.union` Set.fromList
    ([occNameString (rdrNameOcc name) | Thunk expression <- Map.elems (envBindings env),
                                      name <- collect expression :: [RdrName]] ++
     [name | Function name _ <- Map.elems (envBindings env), simpleIdentifier name] ++
     concat (Map.elems (plannedArguments plan)) ++ Map.elems (plannedActions plan) ++
     Map.elems (plannedFields plan) ++ Map.elems (plannedMatches plan) ++ Map.elems (plannedAnchors plan))
  where plan = namePlan env

emitBind :: Env -> LHsBind GhcRn -> Either String O.Decl
emitBind outer binding = case unLoc binding of
    FunBind _ name matches -> do
        unless (arity matches > 0) $
            Left (showSDocUnsafe (ppr (getLocA binding)) ++ ": top-level shared value initialization not implemented yet")
        let params = parameterNames outer (unLoc name) matches
        body <- emitMatches outer (map O.v params) (unLoc (mg_alts matches))
        fname <- case lookupEnv (unLoc name) outer of
            Just (Function n _) -> Right n
            _ -> Left "expected function binding"
        pure (O.val fname (map O.p params) (O.doE [O.thenS tick] body))
    _ -> bindingError binding

-- Only adjacent unary equations with distinct outer constructors share an
-- inspection. Variable/wildcard equations and repeated heads remain barriers.
emitMatches :: Env -> [O.Expr] -> [LMatch GhcRn (LHsExpr GhcRn)] -> Either String O.Expr
emitMatches _ _ [] = Right (failureExpr "source pattern match failure")
emitMatches env [thunk] matches@(located@(L _ match) : _)
    | [pattern'] <- m_pats match,
      (group@(_:_:_), rest) <- adjacentConstructors env matches = do
        fallback <- emitMatches env [thunk] rest
        let next = actionName env "nextEquation" (getLocA located)
            failure = O.v next
            matched = Map.findWithDefault "matched" (patternRoot pattern' 0) (plannedMatches (namePlan env))
        branches <- sequence [constructorFields env thunk pat (patternRoot pat 0)
            (\scope -> withLocals scope (grhssLocalBinds (m_grhss equation)) $ \scope' ->
                emitGuards scope' (grhssGRHSs (m_grhss equation)) failure) failure |
            L _ equation <- group, pat <- m_pats equation]
        let heads = [con | (con,_,_) <- branches]
            alternatives = [(O.conP con (map O.p fields), body) | (con,fields,body) <- branches] ++
                [(O.p "_", failure) | not (exhaustiveConstructors env heads)]
            current = O.doE [O.bind matched (O.call "M.force" [thunk])] (O.caseE (O.v matched) alternatives)
        -- Disjoint heads cannot match after a field/guard failure, so every
        -- branch can use the same ordinary-action continuation after the group.
        pure (if any ((== next) . occNameString . rdrNameOcc) (collect current :: [RdrName])
              then shareAction next fallback current else current)
emitMatches env args (located@(L _ match) : rest) = do
    fallback <- emitMatches env args rest
    let next = actionName env "nextEquation" (getLocA located)
        failure = if null rest then fallback else O.v next
    current <- matchPatterns env (zip args (m_pats match))
        (\scope -> withLocals scope (grhssLocalBinds (m_grhss match)) $ \scope' ->
            emitGuards scope' (grhssGRHSs (m_grhss match)) failure) failure
    pure (if null rest then current else shareAction next fallback current)

adjacentConstructors :: Env -> [LMatch GhcRn (LHsExpr GhcRn)] -> ([LMatch GhcRn (LHsExpr GhcRn)], [LMatch GhcRn (LHsExpr GhcRn)])
adjacentConstructors env = go Set.empty
  where
    go seen (located@(L _ match) : rest)
        | [pattern'] <- m_pats match, Just con <- patternConstructor env pattern', con `Set.notMember` seen =
            let (group, remaining) = go (Set.insert con seen) rest in (located : group, remaining)
    go _ rest = ([], rest)

patternConstructor :: Env -> LPat GhcRn -> Maybe String
patternConstructor env (L _ pattern') = case pattern' of
    ParPat _ nested -> patternConstructor env nested
    AsPat _ _ nested -> patternConstructor env nested
    TuplePat _ [_,_] Boxed -> Just "M.PairT"
    ListPat _ [] -> Just "L.NilT"
    ListPat {} -> Just "L.ConsT"
    ConPat _ name details -> case details of
        PrefixCon {} -> resolvedConstructor env (unLoc name)
        InfixCon {} -> resolvedConstructor env (unLoc name)
        RecCon {} | Just Constructor {} <- lookupEnv (unLoc name) env -> resolvedConstructor env (unLoc name)
        _ -> Nothing
    _ -> Nothing

resolvedConstructor :: Env -> Name -> Maybe String
resolvedConstructor env name
    | Just (Constructor con _ _) <- resolve env name = Just con
    | builtin name "GHC.Types" ":" = Just "L.ConsT"
    | builtin name "GHC.Types" "[]" = Just "L.NilT"
    | builtin name "GHC.Types" "True" = Just "True"
    | builtin name "GHC.Types" "False" = Just "False"
    | otherwise = Nothing

exhaustiveConstructors :: Env -> [String] -> Bool
exhaustiveConstructors env heads = any (all (`elem` heads)) families
  where
    families = [["L.NilT", "L.ConsT"], ["M.NothingT", "M.JustT"], ["False", "True"]] ++
        [cons | tycon <- sourceDatatypes env, let cons = [con | (_,con,_,_) <- constructors [tycon]], not (null cons)]

emitGuards :: Env -> [LGRHS GhcRn (LHsExpr GhcRn)] -> O.Expr -> Either String O.Expr
emitGuards _ [] fallback = Right fallback
emitGuards env [L _ (GRHS _ [] body)] _ = emitExpr env body
emitGuards env (located@(L _ (GRHS _ guards body)) : rest) fallback = do
    next <- emitGuards env rest fallback
    let name = actionName env "nextGuard" (getLocA located)
    current <- go env guards (O.v name)
    pure (shareAction name next current)
  where
    go :: Env -> [GuardLStmt GhcRn] -> O.Expr -> Either String O.Expr
    go scope [] _ = emitExpr scope body
    go scope (L _ (BodyStmt _ expr _ _) : gs) next = do
        condition <- emitExpr scope expr
        remaining <- go scope gs next
        pure (O.doE [O.bind "guardResult" condition] (O.ifE (O.v "guardResult") remaining next))
    go scope (statement@(L _ (BindStmt _ pat expr)) : gs) next = do
        let n = actionName scope "guardThunk" (getLocA statement)
        withMatchThunk scope n expr $ \reference ->
            matchPatterns scope [(reference, pat)] (\scope' -> go scope' gs next) next
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

actionKey :: String -> SrcSpan -> String
actionKey prefix (RealSrcSpan location _) = prefix ++ concatMap (("_" ++) . show)
    [srcSpanStartLine location, srcSpanStartCol location, srcSpanEndLine location, srcSpanEndCol location]
actionKey prefix _ = prefix ++ "_generated"

actionName :: Env -> String -> SrcSpan -> String
actionName env prefix = plannedAction env . actionKey prefix

plannedAction :: Env -> String -> String
plannedAction env key = Map.findWithDefault key key (plannedActions (namePlan env))

matchPatterns :: Env -> [(O.Expr, LPat GhcRn)] -> (Env -> Either String O.Expr) -> O.Expr -> Either String O.Expr
matchPatterns env patterns = matchPatternsAt env
    [(thunk,pattern',patternRoot pattern' index) | (index,(thunk,pattern')) <- zip [0 :: Int ..] patterns]

matchPatternsAt :: Env -> [(O.Expr, LPat GhcRn, String)] -> (Env -> Either String O.Expr) -> O.Expr -> Either String O.Expr
matchPatternsAt env [] success _ = success env
matchPatternsAt env ((thunk, located, path) : rest) success failure = case unLoc located of
    VarPat _ name -> continue (insertEnv (unLoc name) (Thunk thunk) env)
    WildPat _ -> continue env
    ParPat _ pat -> matchPatternsAt env ((thunk, pat, path) : rest) success failure
    AsPat _ name pat -> matchPatternsAt (insertEnv (unLoc name) (Thunk thunk) env) ((thunk, pat, path) : rest) success failure
    TuplePat _ [_,_] Boxed -> constructorCase
    ListPat {} -> constructorCase
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
        matchPatternsAt env ((thunk, noLocA (ListPat noExtField
            (map (noLocA . LitPat noExtField . HsChar NoSourceText) (unpackFS text))), path) : rest) success failure
    ConPat {} -> constructorCase
    _ -> Left (showSDocUnsafe (ppr (getLocA located)) ++ ": pattern not implemented yet")
  where
    continue scope = matchPatternsAt scope rest success failure
    literalCase value body = O.doE [O.bind matchName (O.call "M.force" [thunk])]
        (O.ifE (O.op "==" (O.v matchName) value) body failure)
    constructorCase = do
        (con, fields, body) <- constructorFields env thunk located path continue failure
        pure $ O.doE [O.bind matchName (O.call "M.force" [thunk])] $
            O.caseE (O.v matchName) ((O.conP con (map O.p fields), body) : [(O.p "_",failure) | con `Set.notMember` singleConstructors env])
    matchName = Map.findWithDefault "matched" path (plannedMatches (namePlan env))

-- Extract representation fields once, then use the same source-ordered field
-- traversal for a standalone constructor test or an alternative in a group.
constructorFields :: Env -> O.Expr -> LPat GhcRn -> String -> (Env -> Either String O.Expr) -> O.Expr -> Either String (String, [String], O.Expr)
constructorFields env thunk located path success failure = case unLoc located of
    ParPat _ nested -> constructorFields env thunk nested path success failure
    AsPat _ name nested -> constructorFields (insertEnv (unLoc name) (Thunk thunk) env) thunk nested path success failure
    TuplePat _ [a,b] Boxed -> constructor "M.PairT" [a,b]
    ListPat _ [] -> constructor "L.NilT" []
    ListPat ext (p:ps) -> constructor "L.ConsT" [p, L (getLoc located) (ListPat ext ps)]
    ConPat _ name details -> do
        con <- maybe (Left (showSDocUnsafe (ppr (unLoc name)) ++ ": constructor pattern not implemented yet")) Right
            (resolvedConstructor env (unLoc name))
        case details of
            PrefixCon _ pats -> constructor con pats
            InfixCon a b -> constructor con [a,b]
            RecCon fields | Just (Constructor _ _ labels) <- lookupEnv (unLoc name) env -> do
                let slots = [fieldSlot env (fieldPath path i) pattern' | (i,pattern') <- zip [0..]
                        [maybe (noLocA (WildPat noExtField)) (hfbRHS . unLoc) (lookup label
                            [(fieldName (unLoc (hfbLHS (unLoc f))), f) | f <- rec_flds fields]) | label <- labels]]
                -- Extract in representation order, but perform each supplied
                -- subpattern in source order. Reordering these tests can force
                -- a bottom in a field that a preceding mismatch would skip.
                ordered <- mapM (\f -> do
                    let field = unLoc f
                    (slot,index) <- maybe (Left (showSDocUnsafe (ppr (getLocA located)) ++ ": unknown record pattern field")) Right
                        (lookup (fieldName (unLoc (hfbLHS field))) (zip labels (zip slots [0..])))
                    pure (slot, hfbRHS field, fieldPath path index)) (rec_flds fields)
                body <- matchPatternsAt env [(O.v slot, pat, field) | (slot, pat, field) <- ordered] success failure
                let used = [slot | (slot, pat, _) <- ordered, not (wildcard pat)]
                pure (con, [if slot `elem` used then slot else "_" | slot <- slots], body)
            _ -> Left "unsupported record pattern"
    _ -> Left (showSDocUnsafe (ppr (getLocA located)) ++ ": pattern not implemented yet")
  where
    constructor con pats = do
        let paths = [fieldPath path i | i <- [0 .. length pats - 1]]
            fields = zipWith (fieldSlot env) paths pats
        body <- matchPatternsAt env (zip3 (map O.v fields) pats paths) success failure
        pure (con, fields, body)
    wildcard pat = case unLoc pat of
        WildPat _ -> True
        ParPat _ nested -> wildcard nested
        _ -> False

patternRoot :: LPat GhcRn -> Int -> String
patternRoot pattern' index = actionKey "pattern" (getLocA pattern') ++ "_" ++ show index

fieldPath :: String -> Int -> String
fieldPath path index = path ++ "_field" ++ show index

fieldSlot :: Env -> String -> LPat GhcRn -> String
fieldSlot env path pattern' = case unLoc pattern' of
    VarPat _ name -> readableBinder env (unLoc name)
    AsPat _ name _ -> readableBinder env (unLoc name)
    ParPat _ nested -> fieldSlot env path nested
    WildPat _ -> "_"
    _ -> Map.findWithDefault path path (plannedFields (namePlan env))

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
            functions = [(unLoc n, Function (localName scope (unLoc n)) (arity ms)) | L _ (FunBind _ n ms) <- bindings, arity ms > 0]
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
                let n = localName scope (unLoc name)
                paths <- locatedAt (getLocA name)
                    (maybe (Right Nothing) sharingPaths (Map.lookup (unLoc name) (inferredBindings scope)))
                let reference = if isJust paths then O.call n [O.tuple []] else O.v n
                continuation <- go (insertEnv (unLoc name) (Thunk reference) scope) rest done
                if any ((== n) . occNameString . rdrNameOcc) (collect continuation :: [RdrName])
                    then do
                        let signature = maybeToList (O.sig n <$> Map.lookup (unLoc name) signatures)
                            allocate = maybe (O.v "sharedAt") (\path -> O.call "sharedAtBy" [O.ann (O.v "Proxy") (O.tc "Proxy" [path])]) paths
                            parameters = [O.conP "()" [] | isJust paths]
                            anchorName = Map.findWithDefault (n ++ "anchor") (unLoc name) (plannedAnchors (namePlan scope))
                            declaration = O.val n parameters (O.app allocate [O.call "M.lazy" [action],O.v anchorName])
                        pure (O.doE [O.bind anchorName (O.call "M.lazy" [O.ret (O.tuple [])])] (O.letE (signature ++ [declaration]) continuation))
                    else pure continuation
        _ -> do
            let scope' = insertEnv (unLoc name) (Function (localName scope (unLoc name)) (arity matches)) scope
            declaration <- typedBind scope' binding
            continuation <- go scope' rest done
            pure (O.letE declaration continuation)
    go _ (b:_) _ = bindingError b
    typedBind scope b@(L _ (FunBind _ name _)) = do
        declaration <- emitBind scope b
        pure (maybeToList (O.sig (localName scope (unLoc name)) <$> Map.lookup (unLoc name) signatures) ++ [declaration])
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
            thunks <- sequence [maybe (Right (suspendArgument n (O.ret (failureExpr "source missing record field")))) (emitThunk env n)
                (lookup label [(fieldName (unLoc (hfbLHS (unLoc f))), hfbRHS (unLoc f)) | f <- rec_flds fields]) |
                (label,n) <- zip labels (argumentNames env argumentKey (length labels))]
            unless (length thunks == count) (Left "record construction field layout mismatch")
            pure (lazyArguments thunks (returnConstructor con))
        _ -> unsupported
    HsRecSel _ field -> case resolve env (fieldName field) of
        Just (Function fn _) -> Right (O.ret (O.v fn))
        _ -> unsupported
    HsCase _ scrutinee matches -> do
        let n = actionName env "caseThunk" (getLocA located)
        withMatchThunk env n scrutinee $ \reference ->
            emitMatches env [reference] (unLoc (mg_alts matches))
    ExplicitList _ elements -> list (0 :: Int) elements
    HsApp {} -> let (fun, args) = spine located in case unLoc fun of
        HsVar _ name | isComposition (unLoc name) -> callable
        HsVar _ name | builtin (unLoc name) "GHC.Internal.Err" "error",
            [L _ (HsLit _ (HsString _ text))] <- args -> Right (O.ret (failureExpr (unpackFS text)))
        HsRecSel _ field | Just (fn,_) <- functionResolver env (fieldName field), length args == 1 -> apply fn args
        HsVar _ name -> case resolve env (unLoc name) of
            Just (Constructor con count _) | length args == count -> construct con args
            _ | Just (fn,count) <- functionResolver env (unLoc name), length args <= count -> do
                thunks <- arguments args
                pure (lazyArguments thunks (\names -> let invocation = O.app fn names in
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
    annotate allowFree = annotateExpression env "M.MemoristM" allowFree located
    callable = emitCallable (partialName env) (functionResolver env) (emitThunk env) located
    argumentKey = actionKey "argument" (getLocA located)
    arguments args = zipWithM (emitThunk env) (argumentNames env argumentKey (length args)) args
    apply fn args = do
        thunks <- arguments args
        pure (lazyArguments thunks (O.app fn))
    construct con args = do
        thunks <- arguments args
        pure (lazyArguments thunks (returnConstructor con))
    returnConstructor con names = O.ret (O.call con names)
    list _ [] = Right (O.ret (O.v "L.NilT"))
    list depth (x:xs) = do
        let key = argumentKey ++ "_list" ++ show depth
            names = argumentNames env key 2
        h <- zipWithM (emitThunk env) names [x]
        t <- list (depth + 1) xs
        pure (lazyArguments (h ++ zipWith suspendArgument (drop 1 names) [t]) (returnConstructor "L.ConsT"))
    unsupported = Left (showSDocUnsafe (ppr (getLocA located)) ++ ": unsupported expression " ++ showSDocUnsafe (ppr located))

-- Reuse source bindings at argument, field and matching boundaries, without
-- changing the forcing behavior of emitExpr. A thunk reference can also be a
-- shared factory application rather than a variable in the generated syntax.
emitThunk :: Env -> String -> LHsExpr GhcRn -> Either String ([O.Stmt], O.Expr)
emitThunk env name located = case reuse located of
    Just thunk -> Right ([], thunk)
    Nothing -> suspendArgument name <$> emitExpr env located
  where
    reuse expression = case unLoc expression of
        HsVar _ sourceName | Just (Thunk thunk) <- resolve env (unLoc sourceName) ->
            Just (annotateExpression env "M.Th" False expression thunk)
        HsPar _ nested -> reuse nested
        ExprWithTySig _ nested signature ->
            annotateExpression env "M.Th" (not (hasFreshAnnotationVariables signature)) expression <$> reuse nested
        _ -> Nothing

-- Keep reused scoped annotations at the matching boundary so a nested forall
-- with the same spelling cannot capture their type variables. A plain let binds
-- the existing reference without M.lazy, M.force or ticks; ordinary references
-- and computed subjects keep their existing paths. Protect both matching
-- boundaries before storing aliases in Thunk entries; other Thunk entries are
-- unannotated names or factory references.
withMatchThunk :: Env -> String -> LHsExpr GhcRn -> (O.Expr -> Either String O.Expr) -> Either String O.Expr
withMatchThunk env name located finish = do
    (statements, reference) <- emitThunk env name located
    if null statements && scopedAnnotation located
        then O.letE [O.val name [] reference] <$> finish (O.v name)
        else O.doE statements <$> finish reference
  where
    scopedAnnotation expression = case unLoc expression of
        HsPar _ nested -> scopedAnnotation nested
        ExprWithTySig _ nested signature ->
            (not (hasFreshAnnotationVariables signature) && scopedEvidence expression) || scopedAnnotation nested
        _ -> False
    scopedEvidence expression = case Map.lookup (showSDocUnsafe (ppr (getLocA expression))) (inferredExpressions env) of
        -- renderType emits no type binders. Exclude free source kind variables
        -- that disappear when invisible arguments are omitted in the target.
        Just (True, ty) -> any (isTvOcc . rdrNameOcc) (collect ty :: [RdrName])
        _ -> False

annotateExpression :: Env -> String -> Bool -> LHsExpr GhcRn -> O.Expr -> O.Expr
annotateExpression env wrapper allowFree located code =
    case Map.lookup (showSDocUnsafe (ppr (getLocA located))) (inferredExpressions env) of
        Just (free,ty) | allowFree || not free -> O.ann code (O.tc wrapper [ty])
        _ -> code

suspendArgument :: String -> O.Expr -> ([O.Stmt], O.Expr)
suspendArgument name action = ([O.bind name (O.call "M.lazy" [action])], O.v name)

lazyArguments :: [([O.Stmt], O.Expr)] -> ([O.Expr] -> O.Expr) -> O.Expr
lazyArguments arguments finish = O.doE (concatMap fst arguments) (finish (map snd arguments))

argumentNames :: Env -> String -> Int -> [String]
argumentNames env key count = Map.findWithDefault
    [key ++ "_" ++ show i | i <- [0 .. count - 1]] key (plannedArguments (namePlan env))

planNames :: Data a => Set.Set String -> Map.Map Name [Name] -> a -> NamePlan
planNames globalNames constructorLayouts syntax = NamePlan locals parameters arguments actions fields matchNames anchors
  where
    binds = collect syntax :: [LHsBind GhcRn]
    binders = nub ([unLoc name | L _ (FunBind _ name _) <- binds] ++
        [unLoc name | L _ (VarPat _ name) <- collect syntax :: [LPat GhcRn]] ++
        [unLoc name | L _ (AsPat _ name _) <- collect syntax :: [LPat GhcRn]])
    reserved = globalNames `Set.union` fixedReservedNames
    (locals, afterLocals) = assignBinders reserved binders
    functionParameters = concatMap parametersFor binds
    (parameters, afterParameters) = assignParameters locals afterLocals functionParameters
    argumentSites = concatMap expressionSites (expressions syntax)
    (arguments, afterArguments) = assignArguments afterParameters argumentSites
    actionKeys = nub (
        concat [map (actionKey "nextEquation" . getLocA) (withoutLast (unLoc (mg_alts groupMatches))) |
                groupMatches <- collect syntax :: [MatchGroup GhcRn (LHsExpr GhcRn)]] ++
        [actionKey "nextGuard" (getLocA guard) | guard@(L _ (GRHS _ guards _)) <- collect syntax :: [LGRHS GhcRn (LHsExpr GhcRn)],
                                                     not (null guards)] ++
        [actionKey "caseThunk" (getLocA expression) | expression <- expressions syntax, HsCase {} <- [unLoc expression]] ++
        [actionKey "guardThunk" (getLocA statement) | statement@(L _ BindStmt {}) <- collect syntax :: [GuardLStmt GhcRn]])
    (actions, afterActions) = assignActions afterArguments Map.empty Map.empty actionKeys
    roots = concat [[(pattern',patternRoot pattern' index) | (index,pattern') <- zip [0..] (m_pats match)] |
                    L _ match <- collect syntax :: [LMatch GhcRn (LHsExpr GhcRn)]] ++
        [(pattern',patternRoot pattern' 0) | L _ (BindStmt _ pattern' _) <- collect syntax :: [GuardLStmt GhcRn]]
    fieldKeys = nub (concatMap (uncurry plannedFieldKeys) roots)
    (fields, afterFields) = assignFields afterActions fieldKeys
    matchKeys = nub (concatMap (uncurry plannedMatchKeys) roots)
    (matchNames, afterMatches) = assignPrimed afterFields "matched" matchKeys
    (anchors, _) = assignAnchors locals afterMatches [unLoc name | L _ (FunBind _ name _) <- binds]

    parametersFor (L _ (FunBind _ name functionMatches)) =
        [((unLoc name,i), patternName pattern') |
         (i,pattern') <- zip [0..] (firstPatterns functionMatches)]
    parametersFor _ = []
    patternName (L _ (VarPat _ name)) = Just (unLoc name)
    patternName (L _ (AsPat _ name _)) = Just (unLoc name)
    patternName (L _ (ParPat _ pattern')) = patternName pattern'
    patternName _ = Nothing

    expressionSites expression = case unLoc expression of
        HsApp {} -> [(key, length (snd (spine expression)))]
        OpApp {} -> [(key, 2)]
        ExplicitTuple _ arguments' _ -> [(key, length arguments')]
        RecordCon _ name fields' -> [(key, maybe (length (rec_flds fields')) length
            (Map.lookup (unLoc name) constructorLayouts))]
        ExplicitList _ elements -> [(key ++ "_list" ++ show depth, 2) | depth <- [0 .. length elements - 1]]
        _ -> []
      where key = actionKey "argument" (getLocA expression)

    plannedFieldKeys pattern' path = case unLoc pattern' of
        ParPat _ nested -> plannedFieldKeys nested path
        AsPat _ _ nested -> plannedFieldKeys nested path
        TuplePat _ patterns' Boxed -> children patterns'
        ListPat _ patterns' -> listChildren path patterns'
        LitPat _ (HsString _ text) -> plannedFieldKeys (noLocA (ListPat noExtField
            (map (noLocA . LitPat noExtField . HsChar NoSourceText) (unpackFS text)))) path
        ConPat _ name details -> case details of
            PrefixCon _ patterns' -> children patterns'
            InfixCon left right -> children [left,right]
            RecCon recordFields -> concat
                [child (fieldPath path index) (hfbRHS field) |
                 locatedField <- rec_flds recordFields, let field = unLoc locatedField,
                 Just labels <- [Map.lookup (unLoc name) constructorLayouts],
                 Just index <- [elemIndex (fieldName (unLoc (hfbLHS field))) labels]]
        _ -> []
      where
        children patterns' = concat [child (fieldPath path index) nested |
                                     (index,nested) <- zip [0..] patterns']
        child childPath nested = [childPath | needsField nested] ++ plannedFieldKeys nested childPath
    listChildren _ [] = []
    listChildren path (pattern':patterns') = child (fieldPath path 0) pattern' ++
        [fieldPath path 1 | not (null patterns')] ++ listChildren (fieldPath path 1) patterns'
      where child childPath nested = [childPath | needsField nested] ++ plannedFieldKeys nested childPath
    needsField pattern' = case unLoc pattern' of
        VarPat {} -> False
        AsPat {} -> False
        ParPat _ nested -> needsField nested
        WildPat {} -> False
        _ -> True
    plannedMatchKeys pattern' path = case unLoc pattern' of
        VarPat {} -> []
        WildPat {} -> []
        ParPat _ nested -> plannedMatchKeys nested path
        AsPat _ _ nested -> plannedMatchKeys nested path
        LitPat _ (HsString _ text) -> plannedMatchKeys (noLocA (ListPat noExtField
            (map (noLocA . LitPat noExtField . HsChar NoSourceText) (unpackFS text)))) path
        TuplePat _ patterns' Boxed -> path : matchChildren patterns'
        ListPat _ patterns' -> path : listMatches path patterns'
        ConPat _ constructor details -> path : case details of
            PrefixCon _ patterns' -> matchChildren patterns'
            InfixCon left right -> matchChildren [left,right]
            RecCon fields' -> concat [plannedMatchKeys (hfbRHS field) (fieldPath path index) |
                locatedField <- rec_flds fields', let field = unLoc locatedField,
                Just labels <- [Map.lookup (unLoc constructor) constructorLayouts],
                Just index <- [elemIndex (fieldName (unLoc (hfbLHS field))) labels]]
        _ -> [path]
      where matchChildren patterns' = concat [plannedMatchKeys nested (fieldPath path index) |
                                              (index,nested) <- zip [0..] patterns']
    listMatches _ [] = []
    listMatches path (pattern':patterns') = plannedMatchKeys pattern' (fieldPath path 0) ++
        [fieldPath path 1 | not (null patterns')] ++ listMatches (fieldPath path 1) patterns'
    withoutLast [] = []
    withoutLast [_] = []
    withoutLast (item:items) = item : withoutLast items

firstPatterns :: MatchGroup GhcRn (LHsExpr GhcRn) -> [LPat GhcRn]
firstPatterns matches = case unLoc (mg_alts matches) of
    L _ match : _ -> m_pats match
    [] -> []

assignBinders :: Set.Set String -> [Name] -> (Map.Map Name String, Set.Set String)
assignBinders = go Map.empty
  where
    go assigned used [] = (assigned, used)
    go assigned used (name:names) = go (Map.insert name fresh assigned) (Set.insert fresh used) names
      where
        occurrence = getOccString name
        base = if simpleIdentifier occurrence then occurrence else "local"
        fresh = freshPrimed used base

assignParameters :: Map.Map Name String -> Set.Set String -> [((Name,Int), Maybe Name)] -> (Map.Map (Name,Int) String, Set.Set String)
assignParameters locals = go Map.empty 0
  where
    go assigned _ used [] = (assigned, used)
    go assigned next used ((key,source):rest) = case source >>= (`Map.lookup` locals) of
        Just name -> go (Map.insert key name assigned) next used rest
        Nothing -> let (name,next') = freshNumbered used "arg" next in
            go (Map.insert key name assigned) next' (Set.insert name used) rest

assignArguments :: Set.Set String -> [(String, Int)] -> (Map.Map String [String], Set.Set String)
assignArguments = go Map.empty 0
  where
    go assigned _ used [] = (assigned, used)
    go assigned next used ((key,count):sites) = go (Map.insert key names assigned) next' used' sites
      where
        (names,next',used') = allocate count next used
        allocate 0 number occupied = ([],number,occupied)
        allocate remaining number occupied = let
            (name,following) = freshNumbered occupied "arg" number
            (more,final,occupied') = allocate (remaining - 1) following (Set.insert name occupied)
            in (name:more,final,occupied')

assignActions :: Set.Set String -> Map.Map String String -> Map.Map String Int -> [String] -> (Map.Map String String, Set.Set String)
assignActions used assigned _ [] = (assigned, used)
assignActions used assigned counters (key:keys) = assignActions (Set.insert name used)
    (Map.insert key name assigned) (Map.insert prefix next counters) keys
  where
    prefix
        | "nextEquation" `isPrefixOf` key = "nextEquation"
        | "nextGuard" `isPrefixOf` key = "nextGuard"
        | "caseThunk" `isPrefixOf` key = "caseThunk"
        | otherwise = "guardThunk"
    (name,next) = freshNumbered used prefix (Map.findWithDefault 0 prefix counters)

assignFields :: Set.Set String -> [String] -> (Map.Map String String, Set.Set String)
assignFields = go Map.empty 0
  where
    go assigned _ used [] = (assigned, used)
    go assigned next used (key:keys) = go (Map.insert key name assigned) next' (Set.insert name used) keys
      where (name,next') = freshNumbered used "field" next

assignPrimed :: Set.Set String -> String -> [String] -> (Map.Map String String, Set.Set String)
assignPrimed = go Map.empty
  where
    go assigned used _ [] = (assigned, used)
    go assigned used base (key:keys) = go (Map.insert key name assigned) (Set.insert name used) base keys
      where name = freshPrimed used base

assignAnchors :: Map.Map Name String -> Set.Set String -> [Name] -> (Map.Map Name String, Set.Set String)
assignAnchors locals = go Map.empty
  where
    go assigned used [] = (assigned, used)
    go assigned used (source:sources) = go (Map.insert source name assigned) (Set.insert name used) sources
      where name = freshPrimed used (Map.findWithDefault "local" source locals ++ "anchor")

freshNumbered :: Set.Set String -> String -> Int -> (String, Int)
freshNumbered used prefix number = (freshPrimed used (prefix ++ show number), number + 1)

freshPrimed :: Set.Set String -> String -> String
freshPrimed used base = choose (0 :: Int)
  where
    choose count = let candidate = base ++ replicate count '\'' in
        if candidate `Set.member` used then choose (count + 1) else candidate

simpleIdentifier :: String -> Bool
simpleIdentifier (initial:rest) = (isAlpha initial || initial == '_') &&
    all (\character -> isAlphaNum character || character `elem` "_'") rest
simpleIdentifier [] = False

fixedReservedNames :: Set.Set String
fixedReservedNames = Set.fromList
    [ "M", "L", "Source", "ML", "MI", "MN", "Reflection", "TypeLits", "NE", "QC"
    , "error", "negate", "pure", "Proxy", "sharedAt", "sharedAtBy", "foldrT", "foldlT", "concatMapT"
    , "operand", "condition", "guardResult", "matched"
    , "a", "b", "f", "ta", "tb", "tf", "tx"
    ]

partialName :: Env -> Int -> String
partialName env target = choose target (0 :: Int)
  where
    used = Set.fromList (Map.elems (plannedLocals plan) ++ Map.elems (plannedParameters plan) ++
        concat (Map.elems (plannedArguments plan)) ++ Map.elems (plannedActions plan) ++
        Map.elems (plannedFields plan) ++ Map.elems (plannedMatches plan) ++ Map.elems (plannedAnchors plan))
    plan = namePlan env
    choose remaining count
        | candidate `Set.member` used = choose remaining (count + 1)
        | remaining == 0 = candidate
        | otherwise = choose (remaining - 1) (count + 1)
      where candidate = "partial" ++ replicate count '\''

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
    | builtin name "GHC.Internal.Num" "negate" = Just (adapter ["ta"]
        (O.doE [O.bind "a" (force "ta")] (O.ret (O.call "negate" [O.v "a"]))))
    | any (\m -> builtin name m occurrence) modules = Just (binary
        (O.doE [O.bind "a" (force "ta"), O.bind "b" (force "tb")] (O.ret (O.op occurrence (O.v "a") (O.v "b")))))
    | any (\m -> builtin name m "&&") ["GHC.Classes","GHC.Internal.Classes"] = Just (binary
        (O.doE [O.bind "a" (force "ta")] (O.ifE (O.v "a") (force "tb") (O.ret (O.v "False")))))
    | any (\m -> builtin name m "||") ["GHC.Classes","GHC.Internal.Classes"] = Just (binary
        (O.doE [O.bind "a" (force "ta")] (O.ifE (O.v "a") (O.ret (O.v "True")) (force "tb"))))
    | otherwise = Nothing
  where
    occurrence = getOccString name
    adapter arguments body = Adapter (O.lam (map O.p arguments) body) (length arguments)
    binary = adapter ["ta","tb"]
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
