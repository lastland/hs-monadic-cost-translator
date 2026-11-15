module Example where
difference x y = x-y
folded xs = foldr difference 10 xs
