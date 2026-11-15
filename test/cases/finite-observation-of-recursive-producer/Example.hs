module Example where
stream n = n : stream (n + 1)
second (_:x:_) = x
observe n = second (stream n)
