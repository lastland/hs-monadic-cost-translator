{-# LANGUAGE ScopedTypeVariables #-}
module Example where
f :: a -> Int
f x = 42
g :: a -> Int
g x = f x
parenthesized x = f ((x))
annotated :: forall a. a -> Int
annotated x = f (x :: a)
bad :: a -> Int
bad x = error "computed argument evaluated"
computed x = f (bad x)
