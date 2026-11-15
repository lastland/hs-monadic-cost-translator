module Example where
difference x y = x-y
folded xs = foldl difference 10 xs
