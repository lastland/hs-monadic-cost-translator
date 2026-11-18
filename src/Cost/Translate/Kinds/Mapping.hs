module Cost.Translate.Kinds.Mapping (mapping) where

import Cost.Translate.Output qualified as O

mapping ::
    String ->
    String ->
    [(O.Ty, String, String)] ->
    [(O.Ty, String, String)] ->
    [O.Decl]
mapping name base builtins locals =
    [ O.kindSig (name ++ "Of") (O.forallT [O.b kindName] (O.arrow k k))
    , O.typeSyn (name ++ "Of") [O.bk argumentName k] (O.tc family [k, a])
    , O.kindSig family (O.forallVisible [O.b kindName] (O.arrow k k))
    , O.closedFamily
        family
        (map O.b [kindName, argumentName])
        ( ([O.t "Type", a], O.tc base [a])
            : [ ([kind, O.t source], O.t target)
              | (kind, source, target) <- builtins ++ locals
              ]
            ++ [
                   ( [k, O.tapp f [O.kinded a j]]
                   , O.tc family [O.arrow j k, f, O.tc family [j, a]]
                   )
               , ([k, a], a)
               ]
        )
    ]
    where
        family = name ++ "K"
        kindName = "k"
        argumentName = "a"
        a = O.t argumentName
        f = O.t "f"
        j = O.t "j"
        k = O.t kindName
