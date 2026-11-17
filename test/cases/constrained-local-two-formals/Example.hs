{-# LANGUAGE ScopedTypeVariables #-}
module Example where

work :: Num a => a -> a
work n = n + n

gate :: Num t => t -> Bool
gate _ = True

views :: forall t. Num t => t -> Int -> (((Int, Int), (Int, Int)), ((Int, Integer), (Int, Integer)))
views input count =
    let shared :: forall a b. (Num a, Num b) => (a, b)
        shared = if gate (input :: t)
            then (work (if count < 0 then error "left failure" else 5),
                  work (if count == 0 then error "right failure" else 7))
            else error "root failure"
    in ((shared, shared), (shared, shared))

distinct :: forall t. Num t => t -> ((Int, Int), (Int, Int))
distinct input =
    let first :: forall a b. (Num a, Num b) => (a, b)
        first = if gate (input :: t) then (work 5, work 7) else error "root failure"
        second :: forall a b. (Num a, Num b) => (a, b)
        second = if gate (input :: t) then (work 5, work 7) else error "root failure"
    in (first, second)
