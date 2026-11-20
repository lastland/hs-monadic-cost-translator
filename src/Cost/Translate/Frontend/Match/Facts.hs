module Cost.Translate.Frontend.Match.Facts
    ( Facts
    , Sources
    , scrutinee
    , bindAliases
    , refine
    ) where

import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.Expression.Values (reference)
import Cost.Translate.Output qualified as O
import Cost.Translate.Sharing.Specialization (closedScalar)
import Data.Map.Strict qualified as Map
import GHC

type Facts = Map.Map [Int] O.Expr
type Sources = [([Int], (Name, Type))]

scrutinee :: Env -> LHsExpr GhcRn -> Sources
scrutinee env expr = case reference env expr of
    Just pair@(_, ty) | closedScalar ty -> [([0], pair)]
    _ -> []

bindAliases :: Env -> [Int] -> [Name] -> Sources
bindAliases env pos names =
    [ (pos, (n, ty))
    | n <- names
    , Just ty <- [Map.lookup n (inferredBindings env)]
    , closedScalar ty
    ]

refine :: Facts -> Sources -> Env -> Env
refine facts sources env =
    Map.foldrWithKey bind env (Map.fromListWith (++) groups)
    where
        groups = [(pos, [key]) | (pos, key) <- sources]
        bind pos keys scope =
            let linked = aliasScalars keys scope
             in case Map.lookup pos facts of
                    Nothing -> linked
                    Just value -> foldr (`rememberScalar` value) linked keys
