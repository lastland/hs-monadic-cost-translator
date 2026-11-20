module Cost.Translate.Primitive
    ( Primitive (..)
    , Destination (..)
    , primitive
    , primitiveAdapter
    , saturatedPrimitive
    , locatedPrimitive
    , primitiveBody
    ) where

import Cost.Translate.Callable (spine)
import Cost.Translate.Output qualified as O
import GHC
import GHC.Builtin.Names qualified as Builtin
import GHC.Types.Name (getOccString, nameModule_maybe)

data Primitive = StrictOperator String | Conjunction | Disjunction
data Destination = Returning | Branching O.Expr O.Expr

primitive :: Name -> Maybe Primitive
primitive name
    | from classes "&&" = Just Conjunction
    | from classes "||" = Just Disjunction
    | from modules occurrence = Just (StrictOperator operator)
    | otherwise = Nothing
    where
        occurrence = getOccString name
        from origins symbol =
            occurrence == symbol && maybe False (`elem` origins) origin
        origin = nameModule_maybe name
        classes =
            [ Builtin.gHC_CLASSES
            , Builtin.mkGhcInternalModule_ (mkModuleName "GHC.Internal.Classes")
            ]
        named = ["div", "mod", "quot", "rem"]
        operator
            | occurrence `elem` named = "Prelude." ++ occurrence
            | otherwise = occurrence
        modules
            | occurrence `elem` ["+", "-", "*"] = [Builtin.gHC_INTERNAL_NUM]
            | occurrence `elem` ["==", "/=", "<", ">", "<=", ">="] = classes
            | occurrence `elem` ("/" : named) = [Builtin.gHC_INTERNAL_REAL]
            | otherwise = []

primitiveAdapter :: Name -> Maybe O.Expr
primitiveAdapter name = do
    operation <- primitive name
    let left = "ta"
        right = "tb"
    pure
        ( O.lam [O.p left, O.p right] $
            primitiveBody operation Returning ("a", O.v left) ("b", O.v right)
        )

saturatedPrimitive ::
    LHsExpr GhcRn ->
    Maybe (Name, Primitive, LHsExpr GhcRn, LHsExpr GhcRn)
saturatedPrimitive located = do
    (operator, operation, left, right) <- locatedPrimitive located
    pure (unLoc operator, operation, left, right)

locatedPrimitive ::
    LHsExpr GhcRn ->
    Maybe (Located Name, Primitive, LHsExpr GhcRn, LHsExpr GhcRn)
locatedPrimitive located = do
    (operator, left, right) <- binaryCall located
    HsVar _ name <- pure (unLoc operator)
    operation <- primitive (unLoc name)
    pure (L (getLocA operator) (unLoc name), operation, left, right)

binaryCall ::
    LHsExpr GhcRn -> Maybe (LHsExpr GhcRn, LHsExpr GhcRn, LHsExpr GhcRn)
binaryCall located = case unLoc located of
    OpApp _ left operator right -> Just (operator, left, right)
    HsApp {}
        | (operator, [left, right]) <- spine located ->
            Just (operator, left, right)
    _ -> Nothing

primitiveBody ::
    Primitive -> Destination -> (String, O.Expr) -> (String, O.Expr) -> O.Expr
primitiveBody operation destination (x, left) (y, right) =
    O.doE [O.bind x (force left)] $ case operation of
        StrictOperator operator ->
            O.doE [O.bind y (force right)] (value (O.op operator lhs rhs))
        Conjunction -> action (O.ifE lhs (force right) (O.ret (O.v "False")))
        Disjunction -> action (O.ifE lhs (O.ret (O.v "True")) (force right))
    where
        force thunk = O.call "M.force" [thunk]
        lhs = O.v x
        rhs = O.v y
        value result = case destination of
            Returning -> O.ret result
            Branching yes no -> O.ifE result yes no
        action result = case destination of
            Returning -> result
            Branching yes no -> O.doE [O.bind y result] (O.ifE rhs yes no)
