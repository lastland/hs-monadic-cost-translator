{-# LANGUAGE ScopedTypeVariables #-}
module Example where

work :: Num a => a -> a
work n = n + n

gate :: Num t => t -> Bool
gate _ = True

views :: forall t. Num t => t -> Int -> (((Int,(Int,(Int,Int))),(Int,(Int,(Int,Int)))),((Int,(Int,(Int,Integer))),(Int,(Int,(Int,Integer)))))
views input count =
    let shared :: forall a b c d. (Num a,Num b,Num c,Num d) => (a,(b,(c,d)))
        shared = if gate (input :: t)
            then (work (if count < 0 then error "left failure" else 5),
                 (work 7,(work 9,work (if count == 0 then error "last failure" else 11))))
            else error "root failure"
    in ((shared,shared),(shared,shared))

distinct :: forall t. Num t => t -> ((Int,(Int,(Int,Int))),(Int,(Int,(Int,Int))))
distinct input =
    let first :: forall a b c d. (Num a,Num b,Num c,Num d) => (a,(b,(c,d)))
        first = if gate (input :: t) then (work 5,(work 7,(work 9,work 11))) else error "root failure"
        second :: forall a b c d. (Num a,Num b,Num c,Num d) => (a,(b,(c,d)))
        second = if gate (input :: t) then (work 5,(work 7,(work 9,work 11))) else error "root failure"
    in (first,second)
