module Cost.Translate.Sharing.Reflected (reflectedSharing) where

import Cost.Translate.Output qualified as O
    ( Decl
    , Ty
    , ann
    , arrows
    , b
    , call
    , caseE
    , conP
    , forallT
    , integer
    , letE
    , op
    , p
    , qualified
    , sig
    , t
    , tc
    , v
    , val
    )
import Cost.Translate.Sharing.ReflectionTypes
    ( a
    , evaluateAt
    , keys
    , m
    , th
    )

reflectedSharing :: [O.Decl]
reflectedSharing =
    [ O.sig "sharedAtBy" reflectedType
    , O.val "sharedAtBy" (map O.p ["_", "action", "anchor"]) $
        O.letE
            [ O.val
                "key"
                []
                ( O.call
                    "sharingTypeKey"
                    [keyRepresentation]
                )
            , O.val
                "name"
                []
                ( O.caseE
                    (O.call "M.tname" [O.v "anchor"])
                    [
                        ( O.conP "M.ThName" [O.p "ancestry"]
                        , O.call
                            "M.ThName"
                            [ O.op
                                "NE.:|"
                                (O.integer 0)
                                ( O.op
                                    ":"
                                    (O.integer (-1))
                                    ( O.op
                                        "++"
                                        (O.v "key")
                                        ancestry
                                    )
                                )
                            ]
                        )
                    ]
                )
            ]
            (evaluateAt (O.v "name"))
    ]
    where
        keyRepresentation = O.call "Reflection.someTypeRep" [proxy]
        proxy = O.ann (O.v proxyName) (O.tc proxyName [keys])
        ancestry =
            O.op
                ":"
                (O.integer (-2))
                (O.call "NE.toList" [O.v "ancestry"])

reflectedType :: O.Ty
reflectedType =
    O.forallT (map O.b ["paths", "a"])
        $ O.qualified [O.tc "Reflection.Typeable" [keys]]
        $ O.arrows arguments (th a)
    where
        arguments = [O.tc proxyName [O.t "paths"], m (th a), th (O.t "()")]

proxyName :: String
proxyName = "Proxy"
