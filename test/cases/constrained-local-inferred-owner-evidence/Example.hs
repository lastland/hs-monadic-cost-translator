{-# LANGUAGE ScopedTypeVariables #-}
module Example where

same :: a -> (a, Int) -> (a, Int)
same _ p = p

work :: Num t => t -> t
work n = n + n

views seed count =
  let shared :: forall x y. (Num x, Num y) => (x, y)
      shared = (work (if count < 0 then 3 else 5),
                work (if count == 0 then 11 else 7))
  in (same (seed / 2) shared, same (seed / 2) shared)
