module Cost.Translate.Datatypes.Demand (demandInstances) where

import Cost.Translate.Datatypes.Constructor
import Cost.Translate.Datatypes.Declaration
import Cost.Translate.Output qualified as O

demandInstances :: Datatype -> [O.Decl]
demandInstances datatype =
    [ inst
        datatype
        (constraints datatype "DemandK")
        (O.tc "M.Demand" [target])
        ( O.assoc "Dem" [target] (demandType datatype)
            : map emitFull cs
            ++ map emitUsage cs
            ++ [differentConstructors | length cs > 1]
        )
    , inst
        datatype
        (constraints datatype "DemandK")
        (O.tc "M.LBoundDemand" [target])
        (map emitMinimum cs)
    ]
    where
        target = targetType datatype
        cs = datatypeConstructors datatype
        differentConstructors = O.val "usage" [O.p "_", O.p "_"] (O.v "M.empty")

emitFull :: Constructor -> O.Decl
emitFull c =
    O.val "fullDemand" [patternFor (targetCon c) (fieldNames c)] $
        withWitnesses
            (demandWitnesses c)
            (O.call (demandCon c) [fullField x | x <- fieldNames c])
    where
        fullField x = O.call "M.fullDemand" [O.v x]

emitUsage :: Constructor -> O.Decl
emitUsage c =
    O.val
        "usage"
        [ patternFor (targetCon c) (fieldNames c)
        , patternFor (demandCon c) (map (++ "d") (fieldNames c))
        ]
        $ withWitnesses
            (demandWitnesses c)
            ( foldr
                (O.op "M.union")
                (O.v "M.empty")
                [O.call "M.usage" [O.v x, O.v (x ++ "d")] | x <- fieldNames c]
            )

emitMinimum :: Constructor -> O.Decl
emitMinimum c =
    O.val
        "minDemand"
        [minimumPattern]
        (O.call (demandCon c) (replicate fieldCount (O.v "M.Skip")))
    where
        fieldCount = length (fieldNames c)
        minimumPattern =
            if fieldCount == 0
                then O.conP (targetCon c) []
                else O.conRecordP (targetCon c)
