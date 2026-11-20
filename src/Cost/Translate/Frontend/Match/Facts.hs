module Cost.Translate.Frontend.Match.Facts
    ( Facts
    , Position (..)
    , Sources
    , scrutinee
    , bindAliases
    , refine
    , inheritScalars
    , learnedFacts
    ) where

import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.Expression.Values (reference)
import Cost.Translate.Output qualified as O
import Cost.Translate.Sharing.Specialization (closedScalar)
import Data.Map.Strict qualified as Map
import GHC

data Position = Root Int | Slot Position String Int deriving (Eq, Ord)

type Facts = Map.Map Position O.Expr
type Sources = [(Position, (Name, Type))]

inheritScalars :: Env -> Env -> Env
inheritScalars incoming target =
    target
        { knownScalarValues =
            Map.intersection (knownScalarValues incoming) (envBindings target)
                `Map.union` knownScalarValues target
        }

learnedFacts :: Env -> Sources -> Facts -> Facts
learnedFacts env sources =
    Map.union $
        Map.fromList
            [ (pos, value)
            | (pos, key) <- sources
            , Just value <- [knownScalar env key]
            ]

scrutinee :: Env -> LHsExpr GhcRn -> Sources
scrutinee env expr = case reference env expr of
    Just pair@(_, ty) | closedScalar ty -> [(Root 0, pair)]
    _ -> []

bindAliases :: Env -> Position -> [Name] -> Sources
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
