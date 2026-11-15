{-# LANGUAGE ScopedTypeVariables #-}
module Example where
costly n = n+n
shared :: Num a => a
shared = costly 5
ignore unused x = x
observed x = let go :: forall a. Num a => a -> a; go y = ignore ((shared+shared) :: a) y in go x
