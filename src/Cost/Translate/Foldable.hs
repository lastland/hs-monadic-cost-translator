module Cost.Translate.Foldable (foldableSupport) where

import qualified Cost.Translate.Output as O

-- List methods delegate unchanged. Direct Maybe/pair equations have one entry
-- tick; selecting an instance or passing its callback does not add a tick.
foldableSupport :: [O.Decl]
foldableSupport =
    [ O.classD [] "FoldableT" [O.b "t"]
        [ O.sig "foldrT" (O.arrows [th (O.arrows [th a,th b] (m b)), th b, container] (m b))
        , O.sig "foldlT" (O.arrows [th (O.arrows [th b,th a] (m b)), th b, container] (m b))
        , O.sig "concatMapT" (O.arrows [th (O.arrow (th a) (m (ls b))), container] (m (ls b)))
        , O.val "concatMapT" (map O.p ["tf","txs"]) $ O.doE
            [ O.thenS (O.v "M.tick")
            , O.bind "callback" (O.call "M.lazy" [O.ret (O.lam (map O.p ["x","rest"]) $
                O.doE [O.bind "f" (force "tf"), O.bind "ys" (O.call "M.lazy" [O.call "f" [O.v "x"]])]
                    (O.call "ML.appendM" (map O.v ["ys","rest"])) )])
            , O.bind "nil" (O.call "M.lazy" [O.ret (O.v "L.NilT")]) ]
            (O.call "foldrT" (map O.v ["callback","nil","txs"]))
        ]
    , O.instanceD (O.tc "FoldableT" [O.t "L.ListT"])
        [O.val method [] (O.v runtime) | (method,runtime) <- [("foldrT","ML.foldrM"),("foldlT","ML.foldlM"),("concatMapT","ML.concatMapM")]]
    , O.instanceD (O.tc "FoldableT" [O.t "M.MaybeT"])
        [direct method args [(O.conP "M.NothingT" [], force "z"), (O.conP "M.JustT" [O.p "x"], invoke args)] | (method,args) <- methods]
    , O.instanceD (O.tc "FoldableT" [O.tc "M.PairT" [O.t "c"]])
        [direct method args [(O.conP "M.PairT" (map O.p ["_","x"]), invoke args)] | (method,args) <- methods]
    ]
  where
    a = O.t "a"; b = O.t "b"
    th ty = O.tc "M.Th" [ty]
    m ty = O.tc "M.MemoristM" [ty]
    ls ty = O.tc "L.ListT" [ty]
    container = th (O.tc "t" [a])
    methods = [("foldrT",["x","z"]),("foldlT",["z","x"])]
    force name = O.call "M.force" [O.v name]
    invoke args = O.doE [O.bind "f" (force "tf")] (O.call "f" (map O.v args))
    direct method _ branches = O.val method (map O.p ["tf","z","txs"]) $
        O.doE [O.thenS (O.v "M.tick"), O.bind "xs" (force "txs")] (O.caseE (O.v "xs") branches)
