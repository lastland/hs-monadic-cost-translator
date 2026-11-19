module Cost.Translate.Frontend.Expressions
    ( emitVariable
    , emitLiteral
    , annotateExpression
    , reuseThunk
    , scopedAnnotation
    , suspendArgument
    , lazyArguments
    , returnConstructor
    , failureExpr
    , tick
    , unsupportedExpression
    , unsupportedCall
    , negatedAction
    , conditionalAction
    ) where

import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.Gates (locatedAt)
import Cost.Translate.Frontend.Resolve
import Cost.Translate.Frontend.Signatures (monadicType)
import Cost.Translate.Frontend.Syntax
import Cost.Translate.Output qualified as O
import Cost.Translate.Typed
    ( hasFreeVariables
    , hasFreshAnnotationVariables
    , sourceKey
    )
import Data.Map.Strict qualified as Map
import GHC
import GHC.Builtin.Types (unitDataCon)
import GHC.Types.Name (getOccString, nameModule_maybe)
import GHC.Types.Name.Occurrence (isTvOcc)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

emitLiteral :: Env -> LHsExpr GhcRn -> HsOverLit GhcRn -> O.Expr
emitLiteral env located literal =
    O.ret
        ( maybe
            value
            (O.ann value . O.t)
            (Map.lookup location (inferredLiterals env))
        )
    where
        value = O.overLit (ol_val literal)
        location = sourceKey (getLocA located)

emitVariable :: Env -> LHsExpr GhcRn -> Name -> Either String O.Expr
emitVariable env located name =
    annotate <$> case resolveAt env located name of
        Just (Thunk thunk) -> Right (O.call "M.force" [thunk])
        Just (Function fn 0) -> Right (O.v fn)
        Just (Function fn _) -> Right (O.ret (O.v fn))
        Just (Adapter fn _) -> Right (O.ret fn)
        Just (Constructor con 0 _) -> Right (O.ret (O.v con))
        Just Constructor {} -> unsupportedExpression located
        Just SpecializedThunks {} -> unsupportedExpression located
        Nothing | name == getName unitDataCon -> Right (O.ret (O.tuple []))
        Nothing | sourceType "True" -> trueValue
        Nothing | builtin name "GHC.Internal.Base" "otherwise" -> trueValue
        Nothing | sourceType "False" -> Right (O.ret (O.v "False"))
        Nothing | sourceType "[]" -> Right (O.ret (O.v "L.NilT"))
        Nothing
            | builtin name "GHC.Internal.Err" "undefined" ->
                Right (O.ret (failureExpr "source undefined"))
        Nothing ->
            locatedAt (getLocA located) $
                Left
                    ( "unsupported resolved name "
                        ++ showSDocUnsafe (ppr name)
                        ++ " in "
                        ++ nameScope name
                    )
    where
        annotate = annotateExpression env "M.MemoristM" False located
        sourceType = builtin name "GHC.Types"
        trueValue = Right (O.ret (O.v "True"))

annotateExpression :: Env -> String -> Bool -> LHsExpr GhcRn -> O.Expr -> O.Expr
annotateExpression env wrapper allowFree located code =
    case expressionEvidence env located of
        Just (ty, rendered)
            | allowFree || not (hasFreeVariables ty) ->
                O.ann code (O.tc wrapper [rendered])
        _ -> code

reuseThunk :: Env -> LHsExpr GhcRn -> Maybe O.Expr
reuseThunk env expression = case unLoc expression of
    HsVar _ name
        | Just (Thunk thunk) <- resolveAt env expression (unLoc name) ->
            Just (annotateExpression env "M.Th" False expression thunk)
    HsPar _ nested -> reuseThunk env nested
    ExprWithTySig _ nested signature ->
        annotateExpression
            env
            "M.Th"
            (not (hasFreshAnnotationVariables signature))
            expression
            <$> reuseThunk env nested
    _ -> Nothing

scopedAnnotation :: Env -> LHsExpr GhcRn -> Bool
scopedAnnotation env expression = case unLoc expression of
    HsPar _ nested -> scopedAnnotation env nested
    ExprWithTySig _ nested signature ->
        ( not (hasFreshAnnotationVariables signature)
            && scopedEvidence expression
        )
            || scopedAnnotation env nested
    _ -> False
    where
        scopedEvidence expr = case expressionEvidence env expr of
            Just (ty, rendered)
                | hasFreeVariables ty ->
                    any (isTvOcc . rdrNameOcc) (collect rendered :: [RdrName])
            _ -> False

suspendArgument :: String -> O.Expr -> ([O.Stmt], O.Expr)
suspendArgument name action =
    ([O.bind name (O.call "M.lazy" [action])], O.v name)

lazyArguments :: [([O.Stmt], O.Expr)] -> ([O.Expr] -> O.Expr) -> O.Expr
lazyArguments arguments finish =
    O.doE (concatMap fst arguments) (finish (map snd arguments))

returnConstructor :: String -> [O.Expr] -> O.Expr
returnConstructor con names = O.ret (O.call con names)

failureExpr :: String -> O.Expr
failureExpr message = O.call "error" [O.string message]

tick :: O.Expr
tick = O.ann (O.v "M.tick") (monadicType (O.t "()"))

unsupportedExpression :: LHsExpr GhcRn -> Either String a
unsupportedExpression located =
    locatedAt (getLocA located) $
        Left
            ("unsupported expression " ++ showSDocUnsafe (ppr located))

unsupportedCall :: String -> LHsExpr GhcRn -> Name -> Either String a
unsupportedCall kind located name =
    locatedAt (getLocA located) $
        Left
            ( "unsupported "
                ++ kind
                ++ " "
                ++ getOccString name
                ++ " from "
                ++ nameScope name
            )

nameScope :: Name -> String
nameScope name =
    maybe
        "local scope"
        (moduleNameString . moduleName)
        (nameModule_maybe name)

negatedAction :: O.Expr -> O.Expr
negatedAction action =
    O.doE
        [O.bind "operand" action]
        (O.ret (O.call "negate" [O.v "operand"]))

conditionalAction :: O.Expr -> O.Expr -> O.Expr -> O.Expr
conditionalAction condition yes no =
    O.doE
        [O.bind "condition" condition]
        (O.ifE (O.v "condition") yes no)

expressionEvidence :: Env -> LHsExpr GhcRn -> Maybe (Type, O.Ty)
expressionEvidence env expression = Map.lookup location evidence
    where
        location = sourceKey (getLocA expression)
        evidence = inferredExpressions env
