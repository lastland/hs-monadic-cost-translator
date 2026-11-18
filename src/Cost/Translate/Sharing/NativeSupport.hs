module Cost.Translate.Sharing.NativeSupport (nativeSharingSupport) where

import Cost.Translate.Output qualified as O

nativeSharingSupport :: Bool -> [O.Decl]
nativeSharingSupport lists =
    [ O.sig mapThunkName (O.arrows [O.arrow a b, th a] (th b))
    , O.val
        mapThunkName
        [O.p convertName, O.conP thunkName (map O.p fields)]
        (O.call thunkName [O.v "name", converted, O.v "annotation"])
    ]
        ++ if not lists
            then []
            else
                [ O.sig mapListName (O.arrows [O.arrow a b, list a] (list b))
                , O.val
                    mapListName
                    (map O.p [convertName, "list"])
                    ( O.caseE
                        (O.v "list")
                        [ (O.conP "L.NilT" [], O.v "L.NilT")
                        ,
                            ( O.conP "L.ConsT" (map O.p payloads)
                            , O.call
                                "L.ConsT"
                                [convertedHead, convertedTail]
                            )
                        ]
                    )
                ]
    where
        fields = ["name", "payload", "annotation"]
        payloads = ["headThunk", "tailThunk"]
        converted = O.call convertName [O.v "payload"]
        convertedHead = O.call mapThunkName [O.v convertName, O.v "headThunk"]
        convertedTail = O.call mapThunkName [convertList, O.v "tailThunk"]
        convertList = O.call mapListName [O.v convertName]
        a = O.t "a"
        b = O.t "b"
        th value = O.tc thunkName [value]
        list value = O.tc "L.ListT" [value]

mapThunkName :: String
mapThunkName = "mapThunk"

mapListName :: String
mapListName = "mapListT"

convertName :: String
convertName = "convert"

thunkName :: String
thunkName = "M.Th"
