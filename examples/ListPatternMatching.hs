module ListPatternMatching where

import Prelude hiding (take, zip)

-- The wildcard overlaps the cons pattern. In maybeHeadM the fallback is
-- inlined: the default branch is `_ -> pure M.NothingT`.
maybeHead :: [a] -> Maybe a
maybeHead (x : _) = Just x
maybeHead _       = Nothing

-- The two constructors are disjoint. In maybeHead'M the second branch
-- matches L.NilT explicitly.
maybeHead' :: [a] -> Maybe a
maybeHead' (x : _) = Just x
maybeHead' []      = Nothing

-- The first two equations overlap when both lists are empty. The first
-- equation can return [] without inspecting the second list. When both
-- inputs are cons cells, zipM checks them again in the final equation.
zip :: [a] -> [b] -> [(a, b)]
zip []       _        = []
zip _        []       = []
zip (a : as) (b : bs) = (a, b) : zip as bs

-- These equations are disjoint. Unlike zip, zip' must inspect the second
-- list even when the first list is empty.
zip' :: [a] -> [b] -> [(a, b)]
zip' []       []       = []
zip' []       (_ : _)  = []
zip' (_ : _)  []       = []
zip' (a : as) (b : bs) = (a, b) : zip' as bs

-- A hand translation for the paper can elide the mechanics of < and -
-- after forcing n. The generated takeM shows the thunk allocation and
-- forcing for both literal operands, 0 and 1.
take :: Int -> [a] -> [a]
take n xs =
    if 0 < n then
        case xs of
            []       -> []
            (y : ys) -> y : take (n - 1) ys
    else []
