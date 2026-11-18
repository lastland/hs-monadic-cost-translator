module Cost.Translate.Sharing.Support (sharingSupport) where

import Cost.Translate.Output qualified as O
    ( Decl
    , arrows
    , call
    , p
    , sig
    , t
    , v
    , val
    )
import Cost.Translate.Sharing.Keys (keySupport)
import Cost.Translate.Sharing.Projection (projectionSupport)
import Cost.Translate.Sharing.Reflected (reflectedSharing)
import Cost.Translate.Sharing.ReflectionTypes
    ( a
    , evaluateAt
    , m
    , th
    )

sharingSupport :: Bool -> [O.Decl]
sharingSupport reflected =
    [ O.sig "sharedAt" (O.arrows [m (th a), th (O.t "()")] (th a))
    , O.val
        "sharedAt"
        (map O.p ["action", "anchor"])
        (evaluateAt (O.call "M.tname" [O.v "anchor"]))
    ]
        ++ if reflected
            then projectionSupport ++ reflectedSharing ++ keySupport
            else []
