{-# LANGUAGE ScopedTypeVariables #-}
module Example where

work :: Num a => a -> a
work n = n + n

gate :: Num t => t -> Bool
gate _ = True

views :: forall t. Num t => t -> Int -> (((Int,(Int,Int)),(Int,(Int,Int))),((Int,(Int,Integer)),(Int,(Int,Integer))))
views input count =
    let shared :: forall a b c. (Num a,Num b,Num c) => (a,(b,c))
        shared = if gate (input :: t)
            then (work (if count < 0 then error "left failure" else 5),
                 (work 7,work (if count == 0 then error "last failure" else 9)))
            else error "root failure"
    in ((shared,shared),(shared,shared))

distinct :: forall t. Num t => t -> ((Int,(Int,Int)),(Int,(Int,Int)))
distinct input =
    let first :: forall a b c. (Num a,Num b,Num c) => (a,(b,c))
        first = if gate (input :: t) then (work 5,(work 7,work 9)) else error "root failure"
        second :: forall a b c. (Num a,Num b,Num c) => (a,(b,c))
        second = if gate (input :: t) then (work 5,(work 7,work 9)) else error "root failure"
    in (first,second)
