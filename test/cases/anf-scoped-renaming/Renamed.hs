{-# LANGUAGE ScopedTypeVariables #-}
module Example where

ignore :: t -> Int
ignore _ = 1

work n
  | n == 0 = n + 1
  | otherwise = work (n - n) + 1

shared :: (Num a, Eq a) => a
shared = work 18446744073709551616

caseParameter :: forall a. a -> Int
caseParameter x =
  case (x :: a) of
    y ->
      let inner :: forall b. b -> Int
          inner z = ignore y
      in inner True

guardParameter :: forall a. a -> Int
guardParameter x
  | y <- (x :: a) =
      let inner :: forall b. b -> Int
          inner z = ignore y
      in inner True

caseFactory :: forall a. (Num a, Eq a) => a -> Int
caseFactory x =
  case (shared :: a) of
    y ->
      let inner :: forall b. b -> Int
          inner z = if y == 1 then 1 else 2
      in inner True

guardFactory :: forall a. (Num a, Eq a) => a -> Int
guardFactory x
  | y <- (shared :: a) =
      let inner :: forall b. b -> Int
          inner z = if y == 1 then 1 else 2
      in inner True
