module Example where
add a b = a+b
observed x = head (head (map ((map . add) x) [[x]]))
