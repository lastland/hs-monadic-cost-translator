module Cost.Translate.Sharing.ReflectionTypes where

import Cost.Translate.Output qualified as O
    ( Expr
    , Ty
    , call
    , listT
    , promoted
    , t
    , tc
    , v
    )

a :: O.Ty
a = O.t "a"
f :: O.Ty
f = O.t "f"
k :: O.Ty
k = O.t "k"
value :: O.Ty
value = O.t "value"
rest :: O.Ty
rest = O.t "rest"
typ :: O.Ty
typ = O.t "Type"
steps :: O.Ty
steps = O.listT (O.t "SharingStep")
step :: String -> O.Ty
step name = O.promoted ":" [O.promoted name [], rest]
project :: O.Ty -> O.Ty
project ty = O.tc "ShareProject" [rest, ty]
keys :: O.Ty
keys = O.tc "ShareKeys" [O.t "paths", a]
th :: O.Ty -> O.Ty
th ty = O.tc "M.Th" [ty]
m :: O.Ty -> O.Ty
m ty = O.tc "M.MemoristM" [ty]
evaluateAt :: O.Expr -> O.Expr
evaluateAt name = O.call "M.value" [O.call "M.eval" [O.v "action", name]]
keyOf :: String -> O.Expr
keyOf name = O.call "sharingTypeKey" [representation]
    where
        representation = O.call "Reflection.SomeTypeRep" [O.v name]
