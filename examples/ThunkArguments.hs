{-# LANGUAGE ScopedTypeVariables #-}
module ThunkArguments where

-- These exact argument/capture shapes are the regression inputs. Eta reduction
-- would change their source arities; parentheses exercise transparent wrappers.
{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Redundant bracket" -}

data Box a = Box { first :: a, second :: a }

ignore :: a -> Int
ignore _ = 42

plain, parenthesized :: a -> Int
plain x = ignore x
-- The redundant parentheses are the regression input, not a style example.
parenthesized x = ignore ((x))

annotated :: forall a. a -> Int
annotated x = ignore (x :: a)

bad :: a -> a
bad _ = error "computed argument or field evaluated"

computed :: a -> Int
computed x = ignore (bad x)

constructor, record, mixed :: a -> Box a
constructor x = Box x x
record x = Box { second = (x), first = x }
mixed x = Box x (bad x)

tuple :: a -> (a, a)
tuple x = (x, x)

list :: a -> [a]
list x = [x, x]

cons :: a -> [a] -> [a]
cons x xs = x : xs

pair :: a -> b -> (a, b)
pair x y = (x, y)

keep :: a -> a
keep x = x

partial, composed :: a -> [b] -> [(a, b)]
partial x xs = map (pair x) xs
composed x xs = map (pair x . keep) xs

local :: a -> (a, a)
local x = let shared = keep x in pair shared shared
