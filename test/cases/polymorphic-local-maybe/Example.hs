{-# LANGUAGE ScopedTypeVariables #-}
module Example where

gate :: t -> Bool
gate _ = True

producer :: Int -> Maybe a
producer count = if count < 0 then error "producer root failure"
    else if count == 0 then Nothing else Just (error "producer payload failure")

nestedProducer :: Int -> Maybe (Maybe a)
nestedProducer count = if count < 0 then Nothing else Just (producer count)

views :: forall t. t -> Int -> (Maybe Int, Maybe Bool)
views maybeValue payload =
    let shared :: forall element. Maybe element
        shared = if gate (maybeValue :: t)
                 then (producer payload :: Maybe element) else Nothing
    in (shared, shared)

nested :: forall t. t -> Int -> (Maybe (Maybe Int), Maybe (Maybe Bool))
nested maybeValue payload =
    let shared :: forall element. Maybe (Maybe element)
        shared = if gate (maybeValue :: t)
                 then (nestedProducer payload :: Maybe (Maybe element)) else Nothing
    in (shared, shared)

distinct :: forall t. t -> Int -> (Maybe Int, Maybe Bool)
distinct maybeValue payload =
    let first :: forall element. Maybe element
        first = if gate (maybeValue :: t) then (producer payload :: Maybe element) else Nothing
        second :: forall element. Maybe element
        second = if gate (maybeValue :: t) then (producer payload :: Maybe element) else Nothing
    in (first, second)
