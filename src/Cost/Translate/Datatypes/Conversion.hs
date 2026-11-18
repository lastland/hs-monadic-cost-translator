module Cost.Translate.Datatypes.Conversion
    ( conversionInstance
    , reprDemandInstances
    ) where

import Cost.Translate.Datatypes.Constructor
import Cost.Translate.Datatypes.Declaration
import Cost.Translate.Names (generatedName)
import Cost.Translate.Output qualified as O

conversionInstance :: Datatype -> O.Decl
conversionInstance datatype =
    inst
        datatype
        (map reprRequirement (typeVariables datatype))
        (O.tc "M.Repr" [originalType datatype])
        ( O.assoc
            "Rep"
            [originalType datatype]
            (O.tc name representedVariables)
            : map emitRepr cs
            ++ map emitAbstract cs
        )
    where
        cs = datatypeConstructors datatype
        name = generatedName "T" (datatypeName datatype)
        representedVariables = map represented (typeVariables datatype)

reprDemandInstances :: Datatype -> [O.Decl]
reprDemandInstances datatype =
    [ inst datatype reprDemand (O.tc "M.ReprDemand" [original]) []
    , inst datatype reprDemandArb (O.tc "M.ReprDemandArb" [original]) []
    ]
    where
        vs = typeVariables datatype
        original = originalType datatype
        reprDemand =
            concat [[reprRequirement v, demandRequirement v] | v <- vs]
        demandRequirement v = O.tc "DemandK" [represented v]
        reprDemandArb =
            concat
                [ [reprRequirement v, O.tc "GeneratedK" [represented v]]
                    ++ [ shape cls (O.tc "DemOf" [represented v])
                       | cls <- ["QC.Arbitrary", "Show"]
                       ]
                | v <- vs
                ]

reprRequirement :: O.Ty -> O.Ty
reprRequirement v = O.tc "ReprK" [v]

represented :: O.Ty -> O.Ty
represented v = O.tc "RepOf" [v]

emitRepr :: Constructor -> O.Decl
emitRepr c =
    O.val "reprM" [patternFor (sourceCon c) (fieldNames c)] $
        withWitnesses
            (reprWitnesses c)
            ( applicative
                (targetCon c)
                [O.call "M.lazy" [O.call "M.reprM" [O.v x]] | x <- fieldNames c]
            )

emitAbstract :: Constructor -> O.Decl
emitAbstract c =
    O.val "abstract" [patternFor (targetCon c) (fieldNames c)] $
        withWitnesses
            (reprWitnesses c)
            ( O.call
                (sourceCon c)
                [abstractField x | x <- fieldNames c]
            )
    where
        abstractField x = O.call "M.abstract" [O.call "M.tval" [O.v x]]
