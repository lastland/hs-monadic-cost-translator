{-# LANGUAGE ScopedTypeVariables #-}
module Example where
ignore :: t -> Int
ignore _ = 1
observed :: forall a. a -> Int
observed x = case (x :: a) of y -> let inner :: forall a. a -> Int; inner z = ignore y in inner True
