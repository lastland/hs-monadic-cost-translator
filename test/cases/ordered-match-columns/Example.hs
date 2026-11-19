module Example where

choose :: Int -> Maybe (Int, Int) -> Bool -> (Maybe (Int, Int), Int)
choose _ first@(Just (0, early)) enabled | enabled = (first, early)
choose limit second@(Just (key, later)) _ | later >= limit = (second, key)
choose _ missing@Nothing enabled | enabled = (missing, 17)
choose fallback original _ = (original, fallback)

-- The first row asks only for the guard. After it fails, the right column
-- gets the first chance; its Nothing row cannot skip the intervening left row.
barrier :: Bool -> Maybe Int -> Maybe Int -> Int
barrier stop _ _ | stop = 11
barrier _ _ (Just right) = right
barrier _ (Just left) _ = left
barrier _ _ Nothing = -1

scoped :: Int -> Int -> Bool -> Int
scoped saved offered accept = case offered of
    x | let saved = x + 1, accept -> saved
    _ -> saved

-- A field in one row uses the spelling of another row's untouched argument.
keep :: Int -> Maybe Int -> Int
keep x Nothing = x
keep _ (Just x) | x == 0 = x
keep original (Just _) = original
