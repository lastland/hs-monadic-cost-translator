module Cost.Translate.Sharing.Structural
    ( Slot
    , View
    , planView
    , phantomBranches
    , constructorView
    , viewMapper
    , viewThunk
    , viewDependencies
    ) where

import Control.Monad (guard)
import Cost.Translate.Kinds (visibleArguments)
import Cost.Translate.Names (generatedName)
import Cost.Translate.Output qualified as O
import Data.Map.Strict qualified as Map
import GHC (Name, TyCon, TyVar, Type, getName, isVanillaDataCon, tyConDataCons)
import GHC.Builtin.Types (listTyCon, maybeTyCon, tupleTyCon)
import GHC.Core.DataCon (dataConInstOrigArgTys)
import GHC.Core.TyCo.FVs (tyCoVarsOfType, tyCoVarsOfTypes)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type (getTyVar_maybe, splitTyConApp_maybe)
import GHC.Types.Basic (Boxity (..))
import GHC.Types.Var.Set (elemVarSet)

type Slot = (Name, Int)

data View
    = Preserve
    | Convert O.Expr
    | ListView View
    | MaybeView View
    | PairView View View
    | DatatypeView [(Slot, String, View)]

planView :: Map.Map Slot String -> [(TyVar, O.Expr)] -> Type -> Maybe View
planView mappings variables value
    | not (dependent value) = Just Preserve
    | Just variable <- getTyVar_maybe value =
        Convert <$> lookup variable variables
    | Just (constructor, [element]) <- splitTyConApp_maybe value
    , constructor == listTyCon =
        ListView <$> recurse element
    | Just (constructor, [element]) <- splitTyConApp_maybe value
    , constructor == maybeTyCon =
        MaybeView <$> recurse element
    | Just (constructor, [left, right]) <- splitTyConApp_maybe value
    , constructor == tupleTyCon Boxed 2 =
        PairView <$> recurse left <*> recurse right
    | Just (constructor, arguments) <- splitTyConApp_maybe value = do
        let varying =
                [ ((getName constructor, index), argument)
                | (index, argument) <-
                    zip [0 ..] (visibleArguments constructor arguments)
                , dependent argument
                ]
        guard (not (null varying))
        DatatypeView <$> mapM slot varying
    | otherwise = Nothing
    where
        recurse = planView mappings variables
        dependent ty =
            any ((`elemVarSet` tyCoVarsOfType ty) . fst) variables
        slot (key, argument) = do
            name <- Map.lookup key mappings
            view <- recurse argument
            pure (key, name, view)

phantomBranches :: [TyCon] -> [TyVar] -> Type -> Maybe [(O.Pat, O.Expr)]
phantomBranches datatypes variables result = do
    (tc, arguments) <- splitTyConApp_maybe result
    guard (tc `elem` datatypes && not (null (tyConDataCons tc)))
    mapM (branch arguments) (tyConDataCons tc)
    where
        branch arguments con = do
            guard (isVanillaDataCon con)
            let target = generatedName "T" (getName con)
                fields = map scaledThing (dataConInstOrigArgTys con arguments)
            guard (not (any (`elemVarSet` tyCoVarsOfTypes fields) variables))
            pure (constructorView target (Preserve <$ fields))

constructorView :: String -> [View] -> (O.Pat, O.Expr)
constructorView target views =
    ( O.conP target (map O.p fields)
    , O.call target (zipWith viewThunk views (map O.v fields))
    )
    where
        fields = ["payload" ++ show n | n <- [0 .. length views - 1]]

viewMapper :: View -> O.Expr
viewMapper Preserve = O.v "id"
viewMapper (Convert convert) = convert
viewMapper (ListView view) = O.call "mapListT" [viewMapper view]
viewMapper (MaybeView view) = maybeMapper view
viewMapper (PairView left right) = pairMapper left right
viewMapper (DatatypeView [(_, name, view)]) = O.call name [viewMapper view]
viewMapper (DatatypeView views) =
    O.lam [O.p "structuralValue"] $
        foldl apply (O.v "structuralValue") views
    where
        apply value (_, name, view) = O.call name [viewMapper view, value]

viewThunk :: View -> O.Expr -> O.Expr
viewThunk Preserve payload = payload
viewThunk view payload = O.call "mapThunk" [viewMapper view, payload]

viewDependencies :: View -> [Slot]
viewDependencies Preserve = []
viewDependencies (Convert _) = []
viewDependencies (ListView view) = viewDependencies view
viewDependencies (MaybeView view) = viewDependencies view
viewDependencies (PairView left right) =
    viewDependencies left ++ viewDependencies right
viewDependencies (DatatypeView views) =
    concat [key : viewDependencies view | (key, _, view) <- views]

maybeMapper :: View -> O.Expr
maybeMapper view =
    O.lam
        [O.p "maybeValue"]
        ( O.caseE
            (O.v "maybeValue")
            [ (O.conP "M.NothingT" [], O.v "M.NothingT")
            ,
                ( O.conP "M.JustT" [O.p field]
                , O.call "M.JustT" [viewThunk view (O.v field)]
                )
            ]
        )
    where
        field = "payload"

pairMapper :: View -> View -> O.Expr
pairMapper left right =
    O.lam
        [O.p "pairValue"]
        ( O.caseE
            (O.v "pairValue")
            [
                ( O.conP "M.PairT" [O.p "leftPayload", O.p "rightPayload"]
                , O.call
                    "M.PairT"
                    [ viewThunk left (O.v "leftPayload")
                    , viewThunk right (O.v "rightPayload")
                    ]
                )
            ]
        )
