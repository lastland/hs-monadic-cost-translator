module Cost.Translate.Frontend.Resolve
    ( resolve
    , resolveAt
    , functionResolver
    , remainingArity
    , resolvedConstructor
    , booleanConstant
    ) where

import Cost.Translate.Callable (callArity)
import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.Syntax (builtin)
import Cost.Translate.Output qualified as O
import Cost.Translate.Primitive (primitiveAdapter)
import Cost.Translate.Typed (sourceKey)
import Data.List (find)
import Data.Map.Strict qualified as Map
import GHC
import GHC.Builtin.Names (otherwiseIdName)
import GHC.Builtin.Types (falseDataCon, trueDataCon)
import GHC.Core.TyCo.Compare (eqType)

resolve :: Env -> Name -> Maybe Binding
resolve env name = case lookupEnv name env of
    Just binding -> Just binding
    Nothing -> resolveBuiltin name

resolveBuiltin :: Name -> Maybe Binding
resolveBuiltin name
    | builtin name "GHC.Internal.Maybe" "Just" =
        Just (Constructor "M.JustT" 1 [])
    | builtin name "GHC.Internal.Maybe" "Nothing" =
        Just (Constructor "M.NothingT" 0 [])
    | builtin name listModule "head" = Just (Function "ML.headM" 1)
    | builtin name listModule "tail" = Just (Function "ML.tailM" 1)
    | builtin name listModule "filter" = Just (Function "ML.filterM" 2)
    | builtin name listModule "scanl" = Just (Function "MI.scanlM" 3)
    | builtin name "GHC.Internal.Base" "map" = Just (Function "ML.mapMM" 2)
    | builtin name "GHC.Internal.Base" "foldr" = Just (Function "ML.foldrM" 3)
    | builtin name foldableModule "foldr" = Just (Function "foldrT" 3)
    | builtin name foldableModule "foldl" = Just (Function "foldlT" 3)
    | builtin name foldableModule "concatMap" =
        Just (Function "concatMapT" 2)
    | builtin name listModule "foldl" = Just (Function "ML.foldlM" 3)
    | builtin name listModule "zipWith" = Just (Function "ML.zipWithM" 3)
    | builtin name listModule "concatMap" =
        Just (Function "ML.concatMapM" 2)
    | builtin name "Memorist.Example.NewtonRaphson" "repeatly" =
        Just
            ( Adapter
                ( O.lam
                    (map O.p ["tf", "tx"])
                    ( O.doE
                        [O.bind "f" (O.call "M.force" [O.v "tf"])]
                        (O.call "MN.repeatlyM" [O.v "f", O.v "tx"])
                    )
                )
                2
            )
    | Just adapter <- primitiveAdapter name = Just (Adapter adapter 2)
    | otherwise = Nothing
    where
        listModule = "GHC.Internal.List"
        foldableModule = "GHC.Internal.Data.Foldable"

resolveAt :: Env -> LHsExpr GhcRn -> Name -> Maybe Binding
resolveAt env expression name = case resolve env name of
    Just (SpecializedThunks thunks) -> do
        (ty, _) <-
            Map.lookup
                (sourceKey (getLocA expression))
                (inferredExpressions env)
        Thunk . snd <$> find (eqType ty . fst) thunks
    binding -> binding

remainingArity :: Env -> LHsExpr GhcRn -> Either String Int
remainingArity env = callArity (functionResolver env)

functionResolver :: Env -> Name -> Maybe (O.Expr, Int)
functionResolver env name = case resolve env name of
    Just (Function fn count) -> Just (O.v fn, count)
    Just (Adapter fn count) -> Just (fn, count)
    _ -> Nothing

resolvedConstructor :: Env -> Name -> Maybe String
resolvedConstructor env name
    | Just (Constructor con _ _) <- resolve env name = Just con
    | builtin name typesModule ":" = Just "L.ConsT"
    | builtin name typesModule "[]" = Just "L.NilT"
    | name == getName trueDataCon = Just "True"
    | name == getName falseDataCon = Just "False"
    | otherwise = Nothing
    where
        typesModule = "GHC.Types"

booleanConstant :: Name -> Maybe Bool
booleanConstant name
    | name == otherwiseIdName || name == getName trueDataCon = Just True
    | name == getName falseDataCon = Just False
    | otherwise = Nothing
