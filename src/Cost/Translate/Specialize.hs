module Cost.Translate.Specialize (specializeHelpers) where

import Cost.Translate.Specialize.Analysis
    ( Analysis (candidates)
    , HelperSpecialization (helperQualifiedType, specializedHelper)
    , analyze
    )
import Cost.Translate.Specialize.Selection
    ( specialization
    , substitute
    )
import Cost.Translate.Typed (FunctionType)
import Data.Map.Strict qualified as Map (Map, fromList)
import GHC (GhcRn, HsGroup, Name, Type)

specializeHelpers ::
    HsGroup GhcRn ->
    Map.Map Name FunctionType ->
    Map.Map String Type ->
    (Map.Map Name Type, Type -> Type)
specializeHelpers group checked hints =
    ( Map.fromList
        [ (specializedHelper result, adjust (helperQualifiedType result))
        | result <- planned
        ]
    , adjust
    )
    where
        context = analyze group checked hints
        planned = discover context [] (candidates context)
        adjust = substitute planned

discover ::
    Analysis ->
    [HelperSpecialization] ->
    [(Name, Int, FunctionType)] ->
    [HelperSpecialization]
discover context plan remaining = case next of
    [] -> plan
    result : _ ->
        discover
            context
            (plan ++ [result])
            (filter (notSelected result) remaining)
    where
        next =
            [ result
            | candidate <- remaining
            , Just result <- [specialization context plan candidate]
            ]
        notSelected result (other, _, _) = other /= specializedHelper result
