module Cost.Translate.Foldable (foldableSupport) where

import Cost.Translate.Output qualified as O

foldableSupport :: [O.Decl]
foldableSupport = foldableClass : foldableInstances

foldableClass :: O.Decl
foldableClass =
    O.classD
        []
        foldableName
        [O.b "t"]
        [ O.sig
            foldrName
            (O.arrows [th (O.arrows [th a, th b] (m b)), th b, container] (m b))
        , O.sig
            foldlName
            (O.arrows [th (O.arrows [th b, th a] (m b)), th b, container] (m b))
        , O.sig
            concatMapName
            (O.arrows [th (O.arrow (th a) (m (ls b))), container] (m (ls b)))
        , concatMapDefault
        ]
    where
        a = O.t "a"
        b = O.t "b"
        th ty = O.tc "M.Th" [ty]
        m ty = O.tc "M.MemoristM" [ty]
        ls ty = O.tc listName [ty]
        container = th (O.tc "t" [a])

concatMapDefault :: O.Decl
concatMapDefault =
    O.val concatMapName (map O.p [functionArgument, containerArgument]) $
        O.doE
            [ O.thenS (O.v tickName)
            , O.bind
                "callback"
                ( O.call
                    lazyName
                    [ O.ret
                        ( O.lam (map O.p [elementName, "rest"]) $
                            O.doE
                                [ O.bind functionName (force functionArgument)
                                , O.bind
                                    "ys"
                                    ( O.call
                                        lazyName
                                        [O.call functionName [O.v elementName]]
                                    )
                                ]
                                (O.call "ML.appendM" (map O.v ["ys", "rest"]))
                        )
                    ]
                )
            , O.bind "nil" (O.call lazyName [O.ret (O.v "L.NilT")])
            ]
            (O.call foldrName (map O.v ["callback", "nil", containerArgument]))
    where
        lazyName = "M.lazy"

foldableInstances :: [O.Decl]
foldableInstances =
    [ O.instanceD
        (O.tc foldableName [O.t listName])
        [ O.val method [] (O.v runtime)
        | (method, runtime) <-
            [ (foldrName, "ML.foldrM")
            , (foldlName, "ML.foldlM")
            , (concatMapName, "ML.concatMapM")
            ]
        ]
    , O.instanceD
        (O.tc foldableName [O.t "M.MaybeT"])
        [ direct
            method
            [ (O.conP "M.NothingT" [], force accumulatorName)
            , (O.conP "M.JustT" [O.p elementName], invoke args)
            ]
        | (method, args) <- methods
        ]
    , O.instanceD
        (O.tc foldableName [O.tc "M.PairT" [O.t "c"]])
        [ direct
            method
            [(O.conP "M.PairT" (map O.p ["_", elementName]), invoke args)]
        | (method, args) <- methods
        ]
    ]
    where
        methods =
            [ (foldrName, [elementName, accumulatorName])
            , (foldlName, [accumulatorName, elementName])
            ]
        invoke args =
            O.doE
                [O.bind functionName (force functionArgument)]
                (O.call functionName (map O.v args))
        direct method branches =
            O.val
                method
                (map O.p [functionArgument, accumulatorName, containerArgument])
                $ O.doE
                    [ O.thenS (O.v tickName)
                    , O.bind "xs" (force containerArgument)
                    ]
                    (O.caseE (O.v "xs") branches)

force :: String -> O.Expr
force name = O.call "M.force" [O.v name]

foldableName, foldrName, foldlName, concatMapName, listName :: String
foldableName = "FoldableT"
foldrName = "foldrT"
foldlName = "foldlT"
concatMapName = "concatMapT"
listName = "L.ListT"

functionName, functionArgument, elementName, accumulatorName, tickName :: String
functionName = "f"
functionArgument = "tf"
elementName = "x"
accumulatorName = "z"
tickName = "M.tick"

containerArgument :: String
containerArgument = "txs"
