{-# LANGUAGE ScopedTypeVariables #-}
module Example where

gate :: t -> Bool
gate _ = True

producer :: Int -> (a, b)
producer count = if count < 0 then error "pair root bottom"
                 else (error "pair left bottom", error "pair right bottom")

fixedProducer :: Int -> t -> (a, t)
fixedProducer count input = if count < 0 then error "fixed root bottom"
                           else (error "fixed left bottom", input)

views :: forall t. t -> Int -> ((Int, Bool), (Bool, Int))
views input count =
    let shared :: forall a b. (a, b)
        shared = if gate (input :: t) then (producer count :: (a, b))
                 else (undefined, undefined)
    in (shared, shared)

same :: forall t. t -> Int -> ((Int, Int), (Bool, Bool))
same input count =
    let shared :: forall a. (a, a)
        shared = if gate (input :: t) then (producer count :: (a, a))
                 else (undefined, undefined)
    in (shared, shared)

distinct :: forall t. t -> Int -> ((Int, Bool), (Bool, Int))
distinct input count =
    let first :: forall a b. (a, b)
        first = if gate (input :: t) then (producer count :: (a, b))
                else (undefined, undefined)
        second :: forall a b. (a, b)
        second = if gate (input :: t) then (producer count :: (a, b))
                 else (undefined, undefined)
    in (first, second)

captured :: forall t. t -> Int -> ((Int, t), (Bool, t))
captured input count =
    let shared :: forall a. (a, t)
        shared = if gate (input :: t) then (fixedProducer count input :: (a, t))
                 else (undefined, input)
    in (shared, shared)

leftProducer :: Int -> Maybe [a]
leftProducer count = if count < 0 then error "nested left bottom" else Just []

rightProducer :: Int -> [Maybe (b, a)]
rightProducer count = if count < 0 then error "nested right bottom"
                      else [Just (error "nested pair left bottom", error "nested pair right bottom")]

nested :: forall t. t -> Int -> ((Maybe [Int], [Maybe (Bool, Int)]), (Maybe [Bool], [Maybe (Int, Bool)]))
nested input count =
    let shared :: forall a b. (Maybe [a], [Maybe (b, a)])
        shared = if gate (input :: t) then (leftProducer count, rightProducer count)
                 else (Nothing, [])
    in (shared, shared)
