module Cost.Translate.Frontend.Prune (pruneLocals) where

import Cost.Translate.Frontend.Strict (strictBindNames)
import Cost.Translate.Frontend.Syntax (collect)
import Data.Data (Data, cast, gmapT)
import Data.Graph (graphFromEdges, reachable)
import Data.List (nub)
import Data.Set qualified as Set
import GHC
import GHC.Data.Bag (bagToList, listToBag)
import GHC.Types.Name.Set (elemNameSet)

pruneLocals :: (Data a) => Set.Set Name -> a -> a
pruneLocals strictValues syntax
    | Just (HsLet ext locals body) <- cast nested :: Maybe (HsExpr GhcRn)
    , let kept = keepLocals strictValues body locals
    , Just result <-
        cast (HsLet ext kept body :: HsExpr GhcRn) =
        result
    | Just (GRHSs ext bodies locals) <-
        cast nested :: Maybe (GRHSs GhcRn (LHsExpr GhcRn))
    , Just result <-
        cast
            ( GRHSs ext bodies (keepLocals strictValues bodies locals) ::
                GRHSs GhcRn (LHsExpr GhcRn)
            ) =
        result
    | Just (GRHS ext statements body) <-
        cast nested :: Maybe (GRHS GhcRn (LHsExpr GhcRn))
    , Just result <-
        cast
            ( GRHS ext (pruneGuards strictValues body statements) body ::
                GRHS GhcRn (LHsExpr GhcRn)
            ) =
        result
    | otherwise = nested
    where
        nested = gmapT (pruneLocals strictValues) syntax

references :: (Data a) => a -> [Name]
references value =
    [unLoc name | HsVar _ name <- collect value :: [HsExpr GhcRn]]

keepLocals ::
    (Data a) => Set.Set Name -> a -> HsLocalBinds GhcRn -> HsLocalBinds GhcRn
keepLocals strict body (HsValBinds ext (XValBindsLR (NValBinds groups sigs))) =
    HsValBinds ext (XValBindsLR (NValBinds retained sigs))
    where
        bindings = concatMap (bagToList . snd) groups
        (graph, node, vertex) =
            graphFromEdges
                [ (unLoc name, unLoc name, dependencies)
                | L _ (FunBind free name matches) <- bindings
                , let dependencies =
                        nub
                            (filter (`elemNameSet` free) (references matches))
                ]
        strictNames = filter (`Set.notMember` strict) (strictBindNames bindings)
        live =
            Set.fromList
                [ name
                | root <- strictNames ++ references body
                , Just start <- [vertex root]
                , reached <- reachable graph start
                , let (name, _, _) = node reached
                ]
        wanted (L _ (FunBind _ name _)) = unLoc name `Set.member` live
        wanted _ = True
        retained =
            [ (recursive, listToBag remaining)
            | (recursive, bag) <- groups
            , let remaining = filter wanted (bagToList bag)
            , not (null remaining)
            ]
keepLocals _ _ locals = locals

pruneGuards ::
    Set.Set Name -> LHsExpr GhcRn -> [GuardLStmt GhcRn] -> [GuardLStmt GhcRn]
pruneGuards _ _ [] = []
pruneGuards strictValues body (L location (LetStmt ext locals) : rest) =
    let remaining = pruneGuards strictValues body rest
        kept = keepLocals strictValues (remaining, body) locals
     in L location (LetStmt ext kept)
            : remaining
pruneGuards strictValues body (statement : rest) =
    statement : pruneGuards strictValues body rest
