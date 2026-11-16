module ConstructorEquations where

positive, negative :: Int -> Bool
positive x = x > 0
negative x = x < 0

-- Disjoint constructors should share one inspection of the parameter.
choose :: [Int] -> Int
choose [] = 0
choose (y : _)
    | positive y = y
    | otherwise = 0

-- Repeated constructors still obey equation and guard order.
repeated :: [Int] -> Int
repeated [] = 0
repeated (y : _) | positive y = y
repeated (y : _) | negative y = negate y
repeated (_ : _) = 0

-- A wildcard guard before constructor equations must not inspect the list
-- when the guard succeeds, even when the list itself is bottom.
wildcard :: Bool -> [Int] -> Int
wildcard flag _ | flag = 7
wildcard _ [] = 0
wildcard _ (y : _) = y

headValue :: [Int] -> Int
headValue [] = 0
headValue (y : _) = y

variable :: Bool -> [Int] -> Int
variable _ [] = 0
variable flag whole | flag = headValue whole
variable _ (y : _) = y

-- These unary local helpers capture flag. Constructor grouping must stay
-- behind the first equation's successful, non-strict guard.
guardedWildcard :: Bool -> [Int] -> Int
guardedWildcard flag xs =
    let pick :: [Int] -> Int
        pick _ | flag = 7
        pick [] = 0
        pick (y : _) = y
    in pick xs

guardedVariable :: Bool -> [Int] -> Int
guardedVariable flag xs =
    let pick :: [Int] -> Int
        pick whole | flag = ignoreList whole
        pick [] = 0
        pick (y : _) = y
        ignoreList :: [Int] -> Int
        ignoreList _ = 7
    in pick xs

-- A failed [] guard falls past the Cons equation, rather than entering it.
fallthrough :: Int -> [Int] -> Int
fallthrough n [] | positive n = n
fallthrough _ (y : _) | positive y = y
fallthrough n _ = n

ordered :: Int -> Int
ordered x = choose (x : undefined)
