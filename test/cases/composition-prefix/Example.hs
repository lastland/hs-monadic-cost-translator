module Example where
twice x = x+x
observed x = head (map ((.) twice twice) [x])
