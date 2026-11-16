{-# LANGUAGE ScopedTypeVariables #-}
module ThunkMatching where

-- Preserve the match subjects and alias syntax under test.
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Redundant as-pattern" -}
{- HLINT ignore "Redundant case" -}

plain, parenthesized, asPattern, guardAlias, guardParenthesized, guardAs
    :: a -> (a, a)
plain x = case x of y -> (x, y)
-- Parentheses and irrefutable matches deliberately exercise distinct syntax.
parenthesized x = case ((x)) of y -> (x, y)
asPattern x = case x of y@_ -> (x, y)
guardAlias x | y <- x = (x, y)
guardParenthesized x | y <- ((x)) = (x, y)
guardAs x | y@_ <- x = (x, y)

annotated, guardAnnotated :: forall a. a -> (a, a)
annotated x = case (x :: a) of y -> (x, y)
guardAnnotated x | y <- (x :: a) = (x, y)

work :: Num a => a -> a
work x = x + x

computedCase, computedGuard, local :: Num a => a -> (a, a)
computedCase x = case work x of y -> (y, y)
computedGuard x | y <- work x = (y, y)
local x = let bound = work x in case bound of y -> (bound, y)

sourceLet :: a -> (a, a)
sourceLet x = let bound = x in case bound of y -> (bound, y)

bad :: a -> a
bad _ = error "unused matching subject evaluated"

ignoredCase, ignoredGuard :: a -> a
ignoredCase x = case bad x of _ -> x
ignoredGuard x | _ <- bad x = x

suspendedFailure :: a -> (a, a)
suspendedFailure x = case bad x of y -> (x, y)

shared :: Num a => a
shared = work 5

sameCase, sameGuard :: t -> (Int, Int)
sameCase _ = case (shared :: Int) of y -> (shared :: Int, y)
sameGuard _ | y <- (shared :: Int) = (shared :: Int, y)

different :: t -> (Int, Integer)
different _ | y <- (shared :: Integer) = (shared :: Int, y)

localDictionaries :: t -> (Int, (Int, Integer))
localDictionaries _ =
    let value :: Num a => a
        value = work 5
    in case (value :: Int) of y -> (value :: Int, (y, value :: Integer))

empty :: a -> [b]
empty _ = []

sharedEmpty :: [a]
sharedEmpty = empty (1 :: Int)

polymorphic :: t -> ([Int], [Bool])
polymorphic _ = case (sharedEmpty :: [Int]) of
    ints -> case (sharedEmpty :: [Bool]) of bools -> (ints, bools)

localPolymorphic :: t -> ([Int], [Bool])
localPolymorphic x = let values = empty x in
    case (values :: [Int]) of
        ints -> case (values :: [Bool]) of bools -> (ints, bools)
