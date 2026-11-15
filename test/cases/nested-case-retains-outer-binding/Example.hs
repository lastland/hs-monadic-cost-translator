module Example where
nested x = case x of { y -> case x+x of { z -> y+z } }
