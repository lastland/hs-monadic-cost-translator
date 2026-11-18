module Cost.Translate.Sharing.Keys (keySupport) where

import Cost.Translate.Output qualified as O
    ( Decl
    , arrow
    , call
    , caseE
    , conP
    , integer
    , list
    , listT
    , op
    , p
    , sig
    , t
    , v
    , val
    )
import Cost.Translate.Sharing.ReflectionTypes (keyOf)

keySupport :: [O.Decl]
keySupport =
    [ O.sig
        typeKeyName
        (O.arrow (O.t "Reflection.SomeTypeRep") (O.listT (O.t "Int")))
    , O.val typeKeyName [O.conP "Reflection.SomeTypeRep" [O.p "ty"]] $
        O.caseE
            (O.v "ty")
            [
                ( O.conP "Reflection.App" (map O.p ["f", "a"])
                , O.op consName (O.integer 0) applicationKey
                )
            ,
                ( O.conP "Reflection.Con'" (map O.p ["con", kindsName])
                , O.op
                    consName
                    (O.integer 1)
                    ( O.op
                        "++"
                        ( O.call
                            "concatMap"
                            [ O.v stringKeyName
                            , O.list
                                [ O.call method [O.v "con"]
                                | method <- constructorMethods
                                ]
                            ]
                        )
                        ( O.op
                            consName
                            (O.call "length" [O.v kindsName])
                            kindKeys
                        )
                    )
                )
            ]
    , O.sig stringKeyName (O.arrow (O.t "String") (O.listT (O.t "Int")))
    , O.val
        stringKeyName
        [O.p textName]
        ( O.op
            consName
            (O.call "length" [O.v textName])
            (O.call "map" [O.v "Char.ord", O.v textName])
        )
    ]
    where
        applicationKey = O.op "++" (keyOf "f") (keyOf "a")
        kindKeys = O.call "concatMap" [O.v typeKeyName, O.v kindsName]

constructorMethods :: [String]
constructorMethods =
    [ "Reflection.tyConPackage"
    , "Reflection.tyConModule"
    , "Reflection.tyConName"
    ]

stringKeyName :: String
stringKeyName = "sharingString"

typeKeyName :: String
typeKeyName = "sharingTypeKey"

kindsName :: String
kindsName = "kinds"

textName :: String
textName = "text"

consName :: String
consName = ":"
