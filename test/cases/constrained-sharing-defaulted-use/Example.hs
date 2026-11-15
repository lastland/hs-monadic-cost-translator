module Example where
costly n = n+n
shared :: Num a => a
shared = costly 5
ignore unused x = x
observed x = ignore shared x
