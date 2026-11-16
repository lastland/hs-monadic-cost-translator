{-# LANGUAGE ScopedTypeVariables #-}
-- Shadowing is the regression input in the four explicitly marked functions.
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module ScopedAnnotations where

-- Keep scoped annotations parenthesized in both case and guard subjects.
{- HLINT ignore "Redundant bracket" -}

ignore :: t -> Int
ignore _ = 1

work :: (Num a, Eq a) => a -> a
work n
    | n == 0 = n + 1
    | otherwise = work (n - n) + 1

-- On the supported 64-bit platform this is zero at Int, but nonzero at
-- Integer. Keeping the literal generalized makes the intentional overflow
-- explicit without a concrete overflowing-literal warning.
shared :: (Num a, Eq a) => a
shared = work 18446744073709551616

-- The inner forall deliberately reuses the outer spelling. The captured y
-- must retain its outer type when the translator introduces an annotation.
caseParameter :: forall a. a -> Int
caseParameter x = case (x :: a) of
    y -> let inner :: forall a. a -> Int
             inner _ = ignore y
         in inner True

guardParameter :: forall a. a -> Int
guardParameter x | y <- (x :: a) =
    let inner :: forall a. a -> Int
        inner _ = ignore y
    in inner True

caseFactory :: forall a. (Num a, Eq a) => a -> Int
caseFactory _ = case (shared :: a) of
    y -> let inner :: forall a. a -> Int
             inner _ = if y == 1 then 1 else 2
         in inner True

guardFactory :: forall a. (Num a, Eq a) => a -> Int
guardFactory _ | y <- (shared :: a) =
    let inner :: forall a. a -> Int
        inner _ = if y == 1 then 1 else 2
    in inner True

caseParameterRenamed :: forall a. a -> Int
caseParameterRenamed x = case (x :: a) of
    y -> let inner :: forall b. b -> Int
             inner _ = ignore y
         in inner True

guardFactoryRenamed :: forall a. (Num a, Eq a) => a -> Int
guardFactoryRenamed _ | y <- (shared :: a) =
    let inner :: forall b. b -> Int
        inner _ = if y == 1 then 1 else 2
    in inner True
