module Cost.Translate.Frontend.Emit (emitBind, renderPayload) where

import Control.Monad (unless, void, zipWithM)
import Cost.Translate.Callable (emitCallable, isComposition, spine)
import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.Expression.Arguments
import Cost.Translate.Frontend.Expression.Conditions
import Cost.Translate.Frontend.Expression.Thunks
import Cost.Translate.Frontend.Expression.Values (knownValue, returningScalar)
import Cost.Translate.Frontend.Gates
import Cost.Translate.Frontend.LocalValues
import Cost.Translate.Frontend.Match.Facts (inheritScalars)
import Cost.Translate.Frontend.Match.Fallback (withFallback)
import Cost.Translate.Frontend.NamePlan (NamePlan (..))
import Cost.Translate.Frontend.Patterns
import Cost.Translate.Frontend.Polymorphic
import Cost.Translate.Frontend.Resolve
import Cost.Translate.Frontend.Syntax
import Cost.Translate.Output qualified as O
import Cost.Translate.Primitive (Destination (..))
import Cost.Translate.Sharing (sharingPaths, useTypes)
import Cost.Translate.Typed (hasFreshAnnotationVariables)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import GHC
import GHC.Data.Bag (bagToList)
import GHC.Data.FastString (unpackFS)
import GHC.Types.Basic (Boxity (..), RecFlag (..))

emitBind :: Env -> LHsBind GhcRn -> Either String O.Decl
emitBind outer binding = case unLoc binding of
    FunBind _ name matches -> do
        let params = parameterNames outer (unLoc name) matches
        body <- matchesWith emitMatchBody outer Nothing (map O.v params) matches
        fname <- case lookupEnv (unLoc name) outer of
            Just (Function n _) -> Right n
            _ -> Left "expected function binding"
        pure
            ( O.val
                fname
                (map O.p params)
                (O.doE [O.thenS tick | arity matches > 0] body)
            )
    _ -> bindingError binding

emitGuards ::
    Env -> [LGRHS GhcRn (LHsExpr GhcRn)] -> GuardEnd -> Either String O.Expr
emitGuards env [] fallback = fallback env Right
emitGuards env (located@(L _ (GRHS _ guards body)) : rest) fallback = do
    let failures = scanr ((||) . canMiss) False guards
    unless (or failures) (void (emitGuards env rest fallback))
    emitGuardStatements env body (zip guards (drop 1 failures)) following
    where
        canMiss (L _ statement) = case statement of
            LetStmt {} -> False
            BodyStmt _ expr _ _ -> constantCondition env expr /= Just True
            BindStmt _ pat _ -> not (totalPattern env pat)
            _ -> True
        following scope build = case rest of
            [] -> fallback (inheritScalars scope env) build
            _ -> do
                next <- emitGuards (inheritScalars scope env) rest fallback
                withFallback label next build
        label = actionName env "nextGuard" (getLocA located)

emitMatchBody ::
    Env -> Match GhcRn (LHsExpr GhcRn) -> GuardEnd -> Either String O.Expr
emitMatchBody env match failure =
    withLocals env (grhssLocalBinds rhs) $ \scope ->
        emitGuards scope (grhssGRHSs rhs) failure
    where
        rhs = m_grhss match

emitGuardStatements ::
    Env ->
    LHsExpr GhcRn ->
    [(GuardLStmt GhcRn, Bool)] ->
    GuardEnd ->
    Either String O.Expr
emitGuardStatements scope body [] _
    | probingMatchExit scope = Right (O.tuple [])
    | otherwise = emitExpr scope body
emitGuardStatements env body ((L _ (BodyStmt _ expr _ _), later) : gs) next
    | constantCondition env expr == Just True = yes next env
    | otherwise =
        underCondition emitThunk emitExpr env expr $ \common choose ->
            if later
                then next common $ \miss ->
                    let joined _ build = build miss
                     in choose (yes joined) (const (Right miss))
                else choose (yes next) (`next` Right)
    where
        yes end scope = emitGuardStatements scope body gs end
emitGuardStatements scope body ((s@(L _ (BindStmt _ pat expr)), _) : gs) next =
    withMatchThunk scope (actionName scope "guardThunk" site) expr $ \ref ->
        matchGuard
            scope
            site
            expr
            (ref, pat)
            (\s' -> emitGuardStatements s' body gs)
            next
    where
        site = getLocA s
emitGuardStatements scope body ((L _ (LetStmt _ locals), _) : gs) next =
    withLocals scope locals (\scope' -> emitGuardStatements scope' body gs next)
emitGuardStatements _ _ _ _ = Left "unsupported guard statement"

withLocals ::
    Env ->
    HsLocalBinds GhcRn ->
    (Env -> Either String O.Expr) ->
    Either String O.Expr
withLocals env locals finish = case locals of
    EmptyLocalBinds _ -> finish env
    HsValBinds _ binds -> do
        signatures <- localSignatures env binds
        withTypedValBinds signatures env binds finish
    _ -> Left "local binding form not implemented yet"

withTypedValBinds ::
    Map.Map Name O.Ty ->
    Env ->
    HsValBinds GhcRn ->
    (Env -> Either String O.Expr) ->
    Either String O.Expr
withTypedValBinds sigs env binds finish = case binds of
    XValBindsLR (NValBinds groups _) -> groupsGo env groups
    _ -> Left "unrenamed local bindings"
    where
        groupsGo scope [] = finish scope
        groupsGo scope ((recursive, bs) : rest) = do
            let bindings = bagToList bs
                functions = localFunctions scope bindings
            mapM_ checkLocalBinding bindings
            case recursive of
                Recursive -> do
                    checkRecursiveGroup functions bindings
                    let scope' = extendEnv functions scope
                    declarations <- concat <$> mapM (typedBind scope') bindings
                    continuation <- groupsGo scope' rest
                    pure (O.letE declarations continuation)
                NonRecursive ->
                    emitLocalBindings sigs scope bindings (`groupsGo` rest)
        typedBind scope b@(L _ (FunBind _ name _)) =
            typedDeclarations sigs scope (unLoc name) <$> emitBind scope b
        typedBind _ b = bindingError b

emitLocalBindings ::
    Map.Map Name O.Ty ->
    Env ->
    [LHsBind GhcRn] ->
    (Env -> Either String O.Expr) ->
    Either String O.Expr
emitLocalBindings _ scope [] done = done scope
emitLocalBindings sigs scope (b@(L _ (FunBind _ name matches)) : rest) done =
    case unLoc (mg_alts matches) of
        [L _ match]
            | null (m_pats match) ->
                emitLocalValue
                    sigs
                    scope
                    b
                    name
                    match
                    (\scope' -> emitLocalBindings sigs scope' rest done)
        _ -> do
            let target = localName scope (unLoc name)
                function = Function target (arity matches)
                scope' =
                    insertEnv
                        (unLoc name)
                        function
                        scope
            declaration <-
                typedDeclarations sigs scope' (unLoc name) <$> emitBind scope' b
            continuation <- emitLocalBindings sigs scope' rest done
            pure (O.letE declaration continuation)
emitLocalBindings _ _ (binding : _) _ = bindingError binding

emitLocalValue ::
    Map.Map Name O.Ty ->
    Env ->
    LHsBind GhcRn ->
    LocatedN Name ->
    Match GhcRn (LHsExpr GhcRn) ->
    (Env -> Either String O.Expr) ->
    Either String O.Expr
emitLocalValue signatures scope binding name match done = do
    checkLocalAliases scope match
    let source = unLoc name
        n = localName scope source
        bindingType = Map.lookup source (inferredBindings scope)
        remaining = withLocalThunk scope binding source bindingType (O.v n) done
        payload =
            bindingType >>= (\ty -> constructorPayload scope source ty match)
    case (nativeAlias scope bindingType match, payload) of
        (Just thunk, _) -> do
            rest <- remaining
            let alias = typedDeclarations signatures scope source
            pure (O.letE (alias (O.val n [] thunk)) rest)
        (_, Just plan)
            | let instances = plannedSpecializations (namePlan scope)
            , Map.notMember (unLoc name) instances -> do
                continuation <- remaining
                allocatePayload
                    (renderPayload scope)
                    scope
                    (unLoc name)
                    plan
                    continuation
        _ -> emitOrdinaryLocalValue signatures scope binding name match done

emitOrdinaryLocalValue ::
    Map.Map Name O.Ty ->
    Env ->
    LHsBind GhcRn ->
    LocatedN Name ->
    Match GhcRn (LHsExpr GhcRn) ->
    (Env -> Either String O.Expr) ->
    Either String O.Expr
emitOrdinaryLocalValue signatures scope binding name match done = do
    action <- emitMatchBody scope match $ \_ build ->
        build (failureExpr "source guard failure")
    let source = unLoc name
        n = localName scope source
        bindingType = Map.lookup source (inferredBindings scope)
    case Map.lookup source (plannedSpecializations (namePlan scope)) of
        Just instantiations -> do
            let thunks =
                    [ (ty, O.v target)
                    | (specialization, target) <- instantiations
                    , ty <- useTypes specialization
                    ]
            continuation <-
                done (insertEnv source (SpecializedThunks thunks) scope)
            allocateSpecialized
                signatures
                scope
                name
                action
                bindingType
                instantiations
                continuation
        Nothing -> do
            paths <-
                locatedAt (getLocA name) $
                    maybe (Right Nothing) sharingPaths bindingType
            let reference
                    | isJust paths = O.call n [O.tuple []]
                    | otherwise = O.v n
            let withThunk = withLocalThunk scope binding source bindingType
            continuation <- withThunk reference done
            let producer =
                    bindingType
                        >>= phantomAllocation signatures scope source action
                allocate = case producer of
                    Just bind -> pure . bind
                    Nothing ->
                        allocateShared
                            signatures
                            scope
                            name
                            action
                            bindingType
                            paths
            allocate continuation

emitExpr :: Env -> LHsExpr GhcRn -> Either String O.Expr
emitExpr env located
    | Just act <- returningScalar emitThunk env located = act
emitExpr env located = case unLoc located of
    HsOverLit _ literal -> Right (emitLiteral env located literal)
    HsLit _ (HsChar _ c) -> Right (O.ret (O.char c))
    NegApp _ expression _ -> negatedAction <$> emitExpr env expression
    HsVar _ name
        | Just (_, value) <- knownValue env located -> Right (O.ret value)
        | otherwise -> emitVariable env located (unLoc name)
    HsPar _ expr -> emitExpr env expr
    ExprWithTySig _ expr signature ->
        annotateExpression
            env
            "M.MemoristM"
            (not (hasFreshAnnotationVariables signature))
            located
            <$> emitExpr env expr
    HsIf _ condition yes no ->
        underCondition emitThunk emitExpr env condition $ \_ choose ->
            choose (`emitExpr` yes) (`emitExpr` no)
    HsLet _ locals body -> withLocals env locals (`emitExpr` body)
    ExplicitTuple _ [Present _ a, Present _ b] Boxed ->
        emitConstructor env located "M.PairT" [a, b]
    RecordCon _ name fields -> emitRecord env located (unLoc name) fields
    HsRecSel _ field -> case resolve env (fieldName field) of
        Just (Function fn _) -> Right (O.ret (O.v fn))
        _ -> unsupportedExpression located
    HsCase _ scrutinee matches ->
        let name = actionName env "caseThunk" (getLocA located)
            compile = matchesWith emitMatchBody env (Just scrutinee)
         in withMatchThunk env name scrutinee $ \ref -> compile [ref] matches
    ExplicitList _ elements -> emitList env located 0 elements
    HsApp {} -> emitApplication env located
    OpApp _ left operator right -> emitOperator env located left operator right
    _ -> unsupportedExpression located

renderPayload :: Env -> Payload -> Either String O.Expr
renderPayload = emitPayload emitThunk emitExpr

emitApplication :: Env -> LHsExpr GhcRn -> Either String O.Expr
emitApplication env located = case unLoc fun of
    _ | Just action <- emitSaturated env suspend located Returning -> action
    HsVar _ name | isComposition (unLoc name) -> callable
    HsVar _ name
        | builtin (unLoc name) "GHC.Internal.Err" "error"
        , [L _ (HsLit _ (HsString _ text))] <- args ->
            Right (O.ret (failureExpr (unpackFS text)))
    HsRecSel _ field
        | Just (fn, _) <- functionResolver env (fieldName field)
        , length args == 1 ->
            applyArguments env suspend located (O.app fn) args
    HsVar _ name -> case resolve env (unLoc name) of
        Just (Constructor con count _)
            | length args == count ->
                emitConstructor env located con args
        _
            | Just (fn, count) <- functionResolver env (unLoc name)
            , length args <= count ->
                let invoke names =
                        (if length args == count then id else O.ret)
                            (O.app fn names)
                 in applyArguments env suspend located invoke args
        _ -> unsupportedCall "call" located (unLoc name)
    _ -> callable
    where
        (fun, args) = spine located
        callable = emitCallable (partialName env) resolver suspend located
        suspend = emitThunk env
        resolver = functionResolver env

emitOperator ::
    Env ->
    LHsExpr GhcRn ->
    LHsExpr GhcRn ->
    LHsExpr GhcRn ->
    LHsExpr GhcRn ->
    Either String O.Expr
emitOperator env located left operator right = case unLoc operator of
    _
        | Just action <- emitSaturated env (emitThunk env) located Returning ->
            action
    HsVar _ name
        | isComposition (unLoc name) ->
            emitCallable (partialName env) resolver (emitThunk env) located
    HsVar _ name
        | builtin (unLoc name) "GHC.Types" ":" ->
            emitConstructor env located "L.ConsT" [left, right]
    HsVar _ name
        | Just (Constructor con 2 _) <- resolve env (unLoc name) ->
            emitConstructor env located con [left, right]
    HsVar _ name
        | Just (fn, 2) <- functionResolver env (unLoc name) ->
            applyArguments env (emitThunk env) located (O.app fn) [left, right]
    HsVar _ name -> unsupportedCall "operator" located (unLoc name)
    _ -> unsupportedExpression located
    where
        resolver = functionResolver env

emitRecord ::
    Env -> LHsExpr GhcRn -> Name -> HsRecordBinds GhcRn -> Either String O.Expr
emitRecord env located name fields = case lookupEnv name env of
    Just (Constructor con count labels) -> do
        let names = argumentNames env (argumentKey located) (length labels)
        thunks <-
            sequence
                [ maybe
                    (Right (suspendArgument n missing))
                    (emitField env n)
                    ( lookup
                        label
                        [ (fieldName (unLoc (hfbLHS field)), hfbRHS field)
                        | f <- rec_flds fields
                        , let field = unLoc f
                        ]
                    )
                | (label, n) <- zip labels names
                ]
        unless
            (length thunks == count)
            (Left "record construction field layout mismatch")
        pure (lazyArguments thunks (returnConstructor con))
    _ -> unsupportedExpression located
    where
        missing = O.ret (failureExpr "source missing record field")

emitList ::
    Env -> LHsExpr GhcRn -> Int -> [LHsExpr GhcRn] -> Either String O.Expr
emitList _ _ _ [] = Right (O.ret (O.v "L.NilT"))
emitList env located depth (x : xs) = do
    let key = argumentKey located ++ "_list" ++ show depth
        names = argumentNames env key 2
    h <- zipWithM (emitField env) names [x]
    t <- emitList env located (depth + 1) xs
    pure
        ( lazyArguments
            (h ++ zipWith suspendArgument (drop 1 names) [t])
            (returnConstructor "L.ConsT")
        )

emitThunk :: Env -> String -> LHsExpr GhcRn -> Either String ([O.Stmt], O.Expr)
emitThunk env name located = case reuseThunk env located of
    Just thunk -> Right ([], thunk)
    Nothing -> suspendArgument name <$> emitExpr env located

withMatchThunk ::
    Env ->
    String ->
    LHsExpr GhcRn ->
    (O.Expr -> Either String O.Expr) ->
    Either String O.Expr
withMatchThunk env name located finish = do
    (statements, reference) <- emitThunk env name located
    if null statements && scopedAnnotation env located
        then O.letE [O.val name [] reference] <$> finish (O.v name)
        else O.doE statements <$> finish reference

emitField :: Env -> String -> LHsExpr GhcRn -> Either String ([O.Stmt], O.Expr)
emitField env = fieldThunk env (emitThunk env)

emitConstructor ::
    Env -> LHsExpr GhcRn -> String -> [LHsExpr GhcRn] -> Either String O.Expr
emitConstructor env site con =
    applyArguments env (emitField env) site (returnConstructor con)
