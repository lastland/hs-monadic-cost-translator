module Cost.Translate.Sharing.Projection (projectionSupport) where

import Cost.Translate.Output qualified as O
    ( Decl
    , arrow
    , arrows
    , b
    , closedFamily
    , dataD
    , forallT
    , kindSig
    , kinded
    , listT
    , promoted
    , promotedList
    , t
    , tapp
    , tc
    , tupleT
    )
import Cost.Translate.Sharing.ReflectionTypes
    ( a
    , f
    , k
    , project
    , rest
    , step
    , steps
    , typ
    , value
    )

projectionSupport :: [O.Decl]
projectionSupport =
    [ O.dataD
        "SharingStep"
        []
        [ (name, [])
        | name <-
            [ "SharingFunction"
            , "SharingArgument"
            , "SharingKind"
            , "SharingDomain"
            , "SharingCodomain"
            ]
        ]
    , O.kindSig projectName projectKind
    , O.closedFamily
        projectName
        (map O.b [pathName, "value"])
        [ ([O.promotedList [], value], O.tc "Proxy" [value])
        , ([step "SharingFunction", O.tapp f [a]], project f)
        , ([step "SharingArgument", O.tapp f [a]], project a)
        , ([step "SharingKind", O.kinded value k], project k)
        , ([step "SharingDomain", O.arrow a b], project a)
        , ([step "SharingCodomain", O.arrow a b], project b)
        ]
    , O.kindSig keysName (O.arrows [O.listT steps, typ] typ)
    , O.closedFamily
        keysName
        (map O.b ["paths", "value"])
        [ ([O.promotedList [], value], O.t "()")
        ,
            ( [O.promoted ":" [O.t pathName, rest], value]
            , O.tupleT
                [projected, O.tc keysName [rest, value]]
            )
        ]
    ]
    where
        b = O.t "b"
        projectKind = O.arrow steps (O.forallT [O.b "k"] (O.arrow k typ))
        projected = O.tc projectName [O.t pathName, value]

projectName :: String
projectName = "ShareProject"

pathName :: String
pathName = "path"

keysName :: String
keysName = "ShareKeys"
