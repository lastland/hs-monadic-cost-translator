{-# LANGUAGE ScopedTypeVariables #-}
module TerminalDo where

choose :: Bool -> a -> a -> a
choose c x y = if c then x else y

work :: Int -> Int
work x = x + x

bad :: a -> a
bad _ = error "unselected branch evaluated"

computed :: Bool -> Int -> Int -> Int
computed c x y = choose c (work x) (work y)

ignoredFailure :: Bool -> Int -> Int
ignoredFailure c x = choose c (work x) (bad x)

local :: Bool -> Int -> Int -> Int
local c x y = let saved = work x in choose c saved (work y)

-- The annotation encloses the conditional; the else branch owns its let.
annotated :: forall a. Bool -> a -> a -> a
annotated c x y = (if c then (x :: a) else let saved = y in saved) :: a
