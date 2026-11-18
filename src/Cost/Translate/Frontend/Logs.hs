module Cost.Translate.Frontend.Logs (moduleLogs) where

import Cost.Translate.Callable (expressions, isComposition, spine)
import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.Resolve
import Cost.Translate.Frontend.Syntax
import GHC
import GHC.Types.Name (getOccString)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

moduleLogs :: Env -> HsGroup GhcRn -> String
moduleLogs constructorEnv group =
    unlines
        ( "GHC parsed, renamed and typechecked source;\
          \ lazy A-normalization before optimization"
            : [ sourceLocation (getLocA b)
                    ++ identity (unLoc n)
                    ++ " arity "
                    ++ show (arity ms)
              | b@(L _ (FunBind _ n ms)) <- allBinds
              ]
            ++ concatMap (logExpression logEnv) (expressions group)
        )
    where
        allBinds = collect group :: [LHsBind GhcRn]
        logEnv =
            extendEnv
                [ (unLoc n, Function (getOccString (unLoc n)) (arity ms))
                | L _ (FunBind _ n ms) <- allBinds
                ]
                constructorEnv

logExpression :: Env -> LHsExpr GhcRn -> [String]
logExpression env expr = case unLoc expr of
    HsApp {} -> logApplication env expr
    OpApp _ _ op _
        | HsVar _ name <- unLoc op
        , builtin (unLoc name) "GHC.Internal.Base" "." ->
            [ location
                ++ identity (unLoc name)
                ++ ": composition adapter (zero ticks)"
            ]
    _ -> []
    where
        location = sourceLocation (getLocA expr)

logApplication :: Env -> LHsExpr GhcRn -> [String]
logApplication env expr = case spine expr of
    (L _ (HsVar _ name), _)
        | isComposition (unLoc name) ->
            [ location
                ++ identity (unLoc name)
                ++ ": prefix composition adapter (zero ticks); "
                ++ either id remaining (remainingArity env expr)
            ]
    (L _ (HsVar _ name), args) ->
        [ location
            ++ identity (unLoc name)
            ++ bindingLog (length args) (resolve env (unLoc name))
        ]
    _ -> [location ++ "computed/composed application: validate remaining arity"]
    where
        location = sourceLocation (getLocA expr)
        remaining count = "remaining arity " ++ show count

bindingLog :: Int -> Maybe Binding -> String
bindingLog supplied binding = case binding of
    Just (Function _ count) ->
        suppliedLog count
            ++ if supplied < count
                then "; partial adapter (zero ticks)"
                else "; saturated call"
    Just (Constructor _ count _) ->
        ": constructor fields " ++ show supplied ++ " / " ++ show count
    Just (Adapter _ count) -> suppliedLog count
    _ -> ": calling convention unavailable"
    where
        suppliedLog count =
            ": supplied " ++ show supplied ++ " / arity " ++ show count

sourceLocation :: SrcSpan -> String
sourceLocation location = showSDocUnsafe (ppr location) ++ ": "
