{-# LANGUAGE ScopedTypeVariables #-}
module Example where
costly n = n+n
shared :: Num a => a
shared = costly 5
ignore unused x = x
observed :: forall a. Num a => a -> a
observed x = ignore ((shared+shared) :: a) x
