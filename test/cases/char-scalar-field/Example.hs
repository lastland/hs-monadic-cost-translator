module Example where
data Box = Box Char
readBox (Box c) = 5
observed x = readBox (Box 'a')
