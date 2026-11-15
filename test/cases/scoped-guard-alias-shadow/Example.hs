{-# LANGUAGE ScopedTypeVariables #-}
module Example where
ignore :: t -> Int
ignore _ = 1
observed :: forall a. a -> Int
observed x | y <- (x :: a) = let inner :: forall a. a -> Int; inner z = ignore y in inner True
