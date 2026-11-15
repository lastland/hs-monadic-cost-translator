module Example where
bump x = x+1
mapped xs = map bump xs
add x y = x+y
folded xs = foldr add 0 xs
stream n = n : stream (n+1)
